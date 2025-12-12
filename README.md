# Fuzzy Logic Circuits

A general-purpose Common Lisp library for constructing weighted fuzzy logic networks.

## Overview

**Fuzzy Logic Circuits** provides modular components (gates, inputs, constants), weighted connections, and a simulation environment for building soft logic systems. It implements standard Zadeh fuzzy logic semantics (min/max/1−x) with extensions for weighted inputs and recurrent connectivity.

### Use Cases

- Fuzzy control systems
- Behavior trees and agent decision-making
- Soft logic circuits
- Recurrent fuzzy networks
- Simulated organisms and robotics
- Procedural game AI
- Cognitive architectures requiring graded truth values

---

## Features

- **Core components**: `component`, `connection`, `circuit`
- **Fuzzy gates**: AND (min), OR (max), NOT (1−x), CONSTANT
- **Weighted directed connections** with automatic value clamping
- **Synchronous update model** for stable recurrent networks
- **Value ranges [0.0–1.0]** enforced throughout
- **Simple construction API** for rapid prototyping

---

## Installation

Clone into your Common Lisp project:

```sh
git clone https://github.com/yourname/fuzzy-circuits.git
```

Load the package:

```lisp
(load "fuzzy-circuits.lisp")
(use-package :fuzzy-circuits)
```

Or add an ASDF system definition for integration with Quicklisp.

---

## Core Concepts

### Components

Every node inherits from the `component` class:

- Holds a normalized value in **[0.0, 1.0]**
- Can represent sensors, inputs, constants, or gate outputs
- Has an optional `:name` for debugging and visualization

### Connections

A `connection` links a source component to a target gate:

- **Source**: Any `component` providing a signal
- **Target**: Must be a `fuzzy-gate`
- **Weight**: Optional multiplier (default 1.0), typically in [0.0, 1.0]

Weighted signals are automatically clamped to prevent runaway feedback in recurrent networks.

### Gates

All logic gates inherit from `fuzzy-gate` and implement the `compute` generic function:

- **`and-gate`**: Returns minimum of all weighted inputs
- **`or-gate`**: Returns maximum of all weighted inputs
- **`not-gate`**: Returns 1.0 minus the maximum weighted input
- **`constant-gate`**: Holds a static value (useful for bias nodes)

### Circuit

The `circuit` class manages the collection of components and provides:

- Synchronous batch updates via `compute-step`
- Component registration via `add-component`
- Circuit-wide reset capability

---

## API Reference

### Circuit Management

```lisp
(make-circuit)
;; Creates a new circuit container

(add-component circuit component)
;; Registers a component with the circuit and returns it

(reset-circuit circuit)
;; Resets all component values to 0.0

(compute-step circuit)
;; Performs one synchronous update of all gates
;; Call repeatedly to propagate values through multiple layers
```

### Component Creation

```lisp
(make-instance 'and-gate :name "MyAND")
(make-instance 'or-gate :name "MyOR")
(make-instance 'not-gate :name "MyNOT")
(make-instance 'constant-gate :name "Bias")
(make-instance 'component :name "Sensor")
```

### Connections

```lisp
(connect source target :weight 1.0)
;; Creates a weighted connection from source to target
;; Returns the connection object
```

### Value Manipulation

```lisp
(set-value component value)
;; Sets component value (automatically clamped to [0.0, 1.0])

(get-value component)
;; Returns current component value
```

---

## Example Usage

### Basic Fuzzy Logic Network

```lisp
(defparameter *net* (make-circuit))

;; Create input sensors
(defparameter *sensor-heat*  
  (add-component *net* (make-instance 'component :name "Heat")))
(defparameter *sensor-light* 
  (add-component *net* (make-instance 'component :name "Light")))

;; Create logic gates
(defparameter *danger* 
  (add-component *net* (make-instance 'and-gate :name "Danger")))
(defparameter *safe*   
  (add-component *net* (make-instance 'not-gate :name "Safe")))

;; Wire the network
(connect *sensor-heat*  *danger* :weight 1.0)
(connect *sensor-light* *danger* :weight 1.0)
(connect *danger* *safe*)

;; Set input values
(set-value *sensor-heat* 0.8)
(set-value *sensor-light* 0.9)

;; Run simulation
(compute-step *net*)  ;; First propagation
(compute-step *net*)  ;; Second propagation (for deep networks)

;; Read outputs
(format t "Danger level: ~A~%" (get-value *danger*))  ;; 0.8 (min of inputs)
(format t "Safety level: ~A~%" (get-value *safe*))    ;; 0.2 (1.0 - 0.8)
```

### Weighted Decision Network

```lisp
(defparameter *net* (make-circuit))

;; Multiple factors influencing a decision
(defparameter *hunger* (add-component *net* (make-instance 'component :name "Hunger")))
(defparameter *fatigue* (add-component *net* (make-instance 'component :name "Fatigue")))
(defparameter *threat* (add-component *net* (make-instance 'component :name "Threat")))

;; Decision gate with weighted inputs
(defparameter *should-rest* 
  (add-component *net* (make-instance 'or-gate :name "ShouldRest")))

;; Connect with different weights reflecting priority
(connect *fatigue* *should-rest* :weight 1.0)   ;; High priority
(connect *hunger* *should-rest* :weight 0.3)    ;; Low priority
(connect *threat* *should-rest* :weight 0.1)    ;; Very low priority

;; Simulate scenario
(set-value *hunger* 0.9)
(set-value *fatigue* 0.4)
(set-value *threat* 0.2)

(compute-step *net*)

(format t "Should rest: ~A~%" (get-value *should-rest*))  ;; 0.4 (max of weighted inputs)
```

### Recurrent Feedback Network

```lisp
(defparameter *net* (make-circuit))

(defparameter *input* (add-component *net* (make-instance 'component :name "Input")))
(defparameter *memory* (add-component *net* (make-instance 'or-gate :name "Memory")))

;; Create recurrent connection
(connect *input* *memory* :weight 0.7)
(connect *memory* *memory* :weight 0.5)  ;; Feedback loop

;; Apply brief pulse
(set-value *input* 1.0)
(compute-step *net*)
(format t "Step 1: ~A~%" (get-value *memory*))  ;; 0.7

(set-value *input* 0.0)  ;; Remove input
(compute-step *net*)
(format t "Step 2: ~A~%" (get-value *memory*))  ;; 0.35 (decaying memory)

(compute-step *net*)
(format t "Step 3: ~A~%" (get-value *memory*))  ;; 0.175 (further decay)
```

---

## Design Patterns

### Fuzzy Control Rules

Encode human-interpretable IF-THEN rules using weighted AND/OR logic:

```lisp
;; IF temperature is high AND humidity is high THEN fan-speed is high
(connect *temp-sensor* *fan-controller* :weight 0.8)
(connect *humidity-sensor* *fan-controller* :weight 0.6)
```

### Behavior Arbitration

Blend multiple competing behaviors with smooth transitions instead of hard switches.

### Temporal Integration

Use recurrent connections to create short-term memory and temporal smoothing of noisy inputs.

### Threshold-Free Decisions

Replace brittle boolean conditions with continuous gradations that degrade gracefully.

---

## Technical Notes

### Synchronous Updates

`compute-step` uses a two-phase update:

1. Calculate all new values based on current circuit state
2. Commit all changes simultaneously

This prevents race conditions and ensures deterministic behavior in recurrent networks.

### Value Clamping

All values are automatically clamped to [0.0, 1.0] to maintain stability and prevent overflow in feedback loops.

### NOT Gate Behavior

When a NOT gate receives multiple inputs, it inverts the maximum value among them.

---

## License

MIT

---

## Author

Joshua Boulton  
[jjrboulton.github.io](https://jjrboulton.github.io)

---

## Contributing

Contributions welcome! Please open an issue or pull request on GitHub.
