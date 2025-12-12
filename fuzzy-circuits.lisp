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
   #:constant-gate

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
  (:documentation "Calculates the next value for the gate based on its inputs and returns it."))

;; --- AND (Min) ---
(defclass and-gate (fuzzy-gate component) ()
  (:documentation "Fuzzy AND: Output is the Minimum of all weighted inputs."))

(defmethod compute ((g and-gate))
  (if (gate-inputs g)
      (loop for c in (gate-inputs g) minimize (get-weighted-input c))
      0.0))

;; --- OR (Max) ---
(defclass or-gate (fuzzy-gate component) ()
  (:documentation "Fuzzy OR: Output is the Maximum of all weighted inputs."))

(defmethod compute ((g or-gate))
  (if (gate-inputs g)
      (loop for c in (gate-inputs g) maximize (get-weighted-input c))
      0.0))

;; --- NOT (Inverter) ---
(defclass not-gate (fuzzy-gate component) ()
  (:documentation "Fuzzy NOT: Output is 1.0 - Input."))

(defmethod compute ((g not-gate))
  "Calculates 1.0 minus the maximum weighted input."
  (let ((max-input-val
         (if (gate-inputs g)
             ;; Use the maximum input if multiple sources feed the NOT gate
             (loop for c in (gate-inputs g) maximize (get-weighted-input c))
             0.0)))
    (- 1.0 max-input-val)))

;; --- Constant ---
(defclass constant-gate (component)
    ()
  (:documentation "Holds a static value. Useful for bias nodes."))

;; Constant Gates do not compute a new value from inputs
(defmethod compute ((g constant-gate))
  (component-value g))

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
  "Performs a *synchronous* update of all gates in the circuit.
   All gates calculate their new value based on the previous circuit state,
   and then all new values are committed simultaneously."

  ;; 1. Calculate new values for all fuzzy-gates based on CURRENT state.
  (let ((updates (list)))
    (dolist (comp (circuit-components circuit))
      (when (typep comp 'fuzzy-gate)
            ;; The 'compute' method is modified to *return* the value, not set the slot.
            (let ((new-val (compute comp)))
              ;; Store the component and its calculated new value.
              (push (cons comp new-val) updates))))

    ;; 2. Commit all calculated new values simultaneously.
    (dolist (update updates)
      (setf (component-value (car update)) (cdr update)))))

(defun reset-circuit (circuit)
  "Resets all components to 0.0"
  (dolist (c (circuit-components circuit))
    (setf (component-value c) 0.0)))
