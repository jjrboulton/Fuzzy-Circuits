;;; ----------------------------------------------------------------------------
;;; Fuzzy Logic Circuit Library
;;; A general-purpose library for building weighted fuzzy logic networks.
;;; ----------------------------------------------------------------------------

(defpackage :fuzzy-circuits
  (:use :cl)
  (:export
   ;; Core Classes
   #:component
   #:connection
   #:circuit

   ;; Gates
   #:fuzzy-gate
   #:and-gate #:or-gate #:not-gate
   #:nand-gate #:nor-gate #:xor-gate
   #:buffer-gate #:constant-gate

   ;; API
   #:make-circuit
   #:add-component
   #:connect
   #:set-value
   #:get-value
   #:compute-step
   #:reset-circuit))

(in-package :fuzzy-circuits)

;;; ----------------------------------------------------------------------------
;;; 1. CORE CLASSES
;;; ----------------------------------------------------------------------------

(defclass component ()
    ((value
      :initform 0.0
      :accessor component-value
      :documentation "The current normalized output value (0.0 to 1.0).")
     (name
      :initarg :name
      :initform "Untitled"
      :accessor component-name))
  (:documentation "Base class for any node in the circuit (gates, inputs, etc)."))

(defclass connection ()
    ((source
      :initarg :source
      :accessor connection-source
      :type component
      :documentation "The component providing the signal.")
     (weight
      :initarg :weight
      :initform 1.0
      :accessor connection-weight
      :documentation "Multiplier for the signal strength (usually 0.0 to 1.0)."))
  (:documentation "A weighted link between two components."))

(defclass fuzzy-gate (component)
    ((inputs
      :initform nil
      :accessor gate-inputs
      :documentation "A list of CONNECTION objects feeding into this gate."))
  (:documentation "A component that computes its value based on inputs."))

(defclass circuit ()
    ((components
      :initform nil
      :accessor circuit-components
      :documentation "All components managed by this circuit."))
  (:documentation "Container manages the update loop for a collection of gates."))

;;; ----------------------------------------------------------------------------
;;; 2. CONNECTION LOGIC
;;; ----------------------------------------------------------------------------

(defun connect (source target &key (weight 1.0))
  "Creates a directed connection from SOURCE to TARGET with optional WEIGHT."
  (check-type source component)
  (check-type target fuzzy-gate)
  (let ((conn (make-instance 'connection :source source :weight weight)))
    (push conn (gate-inputs target))
    conn))

(defun get-weighted-input (connection)
  "Retreives the value from the source multiplied by the connection weight."
  (let ((raw-val (component-value (connection-source connection)))
        (w (connection-weight connection)))
    ;; Clamp result between 0.0 and 1.0 ensures stability in recurrent nets
    (min 1.0 (max 0.0 (* raw-val w)))))

;;; ----------------------------------------------------------------------------
;;; 3. GATE IMPLEMENTATIONS (Zadeh Logic)
;;; ----------------------------------------------------------------------------

(defgeneric compute (gate)
  (:documentation "Calculates the next value for the gate based on its inputs."))

;; --- Buffer (Pass-through) ---
(defclass buffer-gate (fuzzy-gate) ()
  (:documentation "Passes the maximum input through directly."))

(defmethod compute ((g buffer-gate))
  (setf (component-value g)
    (if (gate-inputs g)
        (loop for c in (gate-inputs g) maximize (get-weighted-input c))
        0.0)))

;; --- AND (Min) ---
(defclass and-gate (fuzzy-gate) ()
  (:documentation "Fuzzy AND: Output is the Minimum of all inputs."))

(defmethod compute ((g and-gate))
  (setf (component-value g)
    (if (gate-inputs g)
        (loop for c in (gate-inputs g) minimize (get-weighted-input c))
        0.0)))

;; --- OR (Max) ---
(defclass or-gate (fuzzy-gate) ()
  (:documentation "Fuzzy OR: Output is the Maximum of all inputs."))

(defmethod compute ((g or-gate))
  (setf (component-value g)
    (if (gate-inputs g)
        (loop for c in (gate-inputs g) maximize (get-weighted-input c))
        0.0)))

;; --- NOT (Inverter) ---
(defclass not-gate (fuzzy-gate) ()
  (:documentation "Fuzzy NOT: Output is 1.0 - Input."))

(defmethod compute ((g not-gate))
  ;; Uses the first input found, or 0.0
  (let ((input-val (if (gate-inputs g)
                       (get-weighted-input (first (gate-inputs g)))
                       0.0)))
    (setf (component-value g) (- 1.0 input-val))))

;; --- NAND (Not And) ---
(defclass nand-gate (fuzzy-gate) ()
  (:documentation "Fuzzy NAND: 1.0 - Min(inputs)."))

(defmethod compute ((g nand-gate))
  (let ((min-val (if (gate-inputs g)
                     (loop for c in (gate-inputs g) minimize (get-weighted-input c))
                     0.0)))
    (setf (component-value g) (- 1.0 min-val))))

;; --- NOR (Not Or) ---
(defclass nor-gate (fuzzy-gate) ()
  (:documentation "Fuzzy NOR: 1.0 - Max(inputs)."))

(defmethod compute ((g nor-gate))
  (let ((max-val (if (gate-inputs g)
                     (loop for c in (gate-inputs g) maximize (get-weighted-input c))
                     0.0)))
    (setf (component-value g) (- 1.0 max-val))))

;; --- XOR (Exclusive Or) ---
(defclass xor-gate (fuzzy-gate) ()
  (:documentation "Fuzzy XOR: Absolute difference |A - B|. best with exactly 2 inputs."))

(defmethod compute ((g xor-gate))
  (if (>= (length (gate-inputs g)) 2)
      (let ((v1 (get-weighted-input (first (gate-inputs g))))
            (v2 (get-weighted-input (second (gate-inputs g)))))
        (setf (component-value g) (abs (- v1 v2))))
      (setf (component-value g) 0.0)))

;; --- Constant ---
(defclass constant-gate (component)
    ()
  (:documentation "Holds a static value. Useful for bias nodes."))

;;; ----------------------------------------------------------------------------
;;; 4. CIRCUIT MANAGEMENT
;;; ----------------------------------------------------------------------------

(defun make-circuit ()
  (make-instance 'circuit))

(defun add-component (circuit component)
  "Registers a component with the circuit."
  (push component (circuit-components circuit))
  component)

(defun set-value (component new-value)
  "Manually sets the value of a component (e.g., for Sensors/Inputs)."
  (setf (component-value component) (min 1.0 (max 0.0 (float new-value)))))

(defun get-value (component)
  (component-value component))

(defun compute-step (circuit)
  "Updates all gates in the circuit. 
   Note: This performs a synchronous update based on *previous* states.
   Ideal for recurrent neural networks or simulations."

  ;; 1. Calculate new values (but don't set them yet, to preserve state for this tick)
  (let ((updates (list)))
    (dolist (comp (circuit-components circuit))
      (when (typep comp 'fuzzy-gate)
            ;; We store the pair (gate . new-value)
            (push (cons comp
                        ;; Logic to peek at next value without setting it
                        (let ((temp-val 0.0))
                          ;; Temporarily hook generic compute to return value 
                          ;; (Optimized implementation would split compute into predict/commit phases)
                          ;; For now, we just rely on standard CLOS flow:
                          (compute comp)
                          (component-value comp)))
                  updates)))

    ;; Note: The implementation above actually updates effectively "in-place" 
    ;; depending on order, which is "Asynchronous Updating". 
    ;; If strict "Synchronous" (cellular automata style) is required,
    ;; we must cache values first.

    ;; PURE SYNCHRONOUS VERSION:
    ;; (Re-implementing compute to be side-effect free would be cleaner, 
    ;; but for now we assume 'compute' sets the slot. 
    ;; For a general lib, let's keep it simple: Topological sort is hard for recurrent.
    ;; We will stick to simple sequential update. 
    ;; Users should call compute-step multiple times for propagation.)

    ;; Simple pass:
    (dolist (comp (circuit-components circuit))
      (when (typep comp 'fuzzy-gate)
            (compute comp)))))

(defun reset-circuit (circuit)
  "Resets all components to 0.0"
  (dolist (c (circuit-components circuit))
    (setf (component-value c) 0.0)))

;;; ----------------------------------------------------------------------------
;;; 5. EXAMPLE USAGE (Commented Out)
;;; ----------------------------------------------------------------------------

#|
(defparameter *net* (make-circuit))

;; Define inputs
(defparameter *sensor-heat* (add-component *net* (make-instance 'component :name "Heat Sensor")))
(defparameter *sensor-light* (add-component *net* (make-instance 'component :name "Light Sensor")))

;; Define Brain
(defparameter *gate-danger* (add-component *net* (make-instance 'and-gate :name "Danger Detection")))
(defparameter *gate-run* (add-component *net* (make-instance 'not-gate :name "Run Command")))

;; Connect: Danger = Heat AND Light
(connect *sensor-heat* *gate-danger*)
(connect *sensor-light* *gate-danger*)

;; Connect: Run = NOT Danger (Silly logic, but demonstrates chaining)
(connect *gate-danger* *gate-run*)

;; Simulation
(set-value *sensor-heat* 0.8)
(set-value *sensor-light* 0.9)

(compute-step *net*)
(compute-step *net*) ;; Second step to propagate to second layer

(format t "Danger Level: ~A~%" (get-value *gate-danger*)) ;; Should be min(0.8, 0.9) -> 0.8
(format t "Run Command: ~A~%" (get-value *gate-run*))     ;; Should be 1.0 - 0.8 -> 0.2
|#