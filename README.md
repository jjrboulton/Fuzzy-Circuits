# Fuzzy Logic Circuits --- Documentation

## Overview

**Fuzzy Logic Circuits** is a general‑purpose Common Lisp library for constructing weighted fuzzy logic networks. It provides modular components (gates, inputs, constants), weighted connections, and a simulation environment suitable for:

-   Fuzzy control systems
-   Behaviour trees and agent decision‑making
-   Soft logic circuits
-   Recurrent fuzzy networks
-   Low‑precision continuous neural models
-   Simulated organisms and robotics
-   Procedural game AI
-   Cognitive architectures requiring graded truth values

The system follows standard Zadeh fuzzy logic semantics (min/max/1−x),
with extensions such as weighted inputs and recurrent connectivity.

------------------------------------------------------------------------

## Features

-   **Core components**: `component`, `connection`, `circuit`
-   **Fuzzy gates**: AND, OR, NOT, NAND, NOR, XOR, BUFFER, CONSTANT
-   **Weighted directed connections**
-   **Synchronous or iterative update model**
-   **Clamped value ranges (0.0--1.0)** for stability
-   **Simple construction API**

------------------------------------------------------------------------

## Installation

Clone into your Common Lisp project:

``` sh
git clone https://github.com/yourname/fuzzy-circuits.git
```

Load via ASDF or Quicklisp (if you add a system file).

------------------------------------------------------------------------

## Core Concepts

### Components

Every node in a circuit inherits from `component`.

-   Holds a value in **\[0.0, 1.0\]**
-   May represent sensors, constants, or gate outputs

### Connections

A `connection` links a **source component** to a **target gate**, with an optional **weight**.

Weighted signals are clamped, preventing runaway feedback in recurrent
nets.

### Gates

All logical gates inherit from `fuzzy-gate` and implement:

    (compute gate)

The supported gates include:

-   `and-gate` --- min of inputs
-   `or-gate` --- max of inputs
-   `not-gate` --- 1 − input
-   `nand-gate`, `nor-gate`, `xor-gate`
-   `buffer-gate` --- passthrough
-   `constant-gate` --- fixed value

------------------------------------------------------------------------

## Circuit Management

### Creating a circuit

``` lisp
(defparameter *c* (make-circuit))
```

### Adding components

``` lisp
(add-component *c* (make-instance 'and-gate :name "Example"))
```

### Connecting components

``` lisp
(connect source target :weight 0.7)
```

### Setting and reading values

``` lisp
(set-value sensor 0.9)
(get-value gate)
```

### Running a simulation step

``` lisp
(compute-step *c*)
```

Call repeatedly to allow values to propagate through multiple layers.

### Resetting

``` lisp
(reset-circuit *c*)
```

------------------------------------------------------------------------

## Example

``` lisp
(defparameter *net* (make-circuit))

;; Inputs
(defparameter *sensor-heat*  (add-component *net* (make-instance 'component :name "Heat")))
(defparameter *sensor-light* (add-component *net* (make-instance 'component :name "Light")))

;; Gates
(defparameter *danger* (add-component *net* (make-instance 'and-gate :name "Danger")))
(defparameter *run*    (add-component *net* (make-instance 'not-gate :name "Run")))

;; Wiring
(connect *sensor-heat*  *danger*)
(connect *sensor-light* *danger*)
(connect *danger*        *run*)

;; Simulation
(set-value *sensor-heat* 0.8)
(set-value *sensor-light* 0.9)

(compute-step *net*)
(compute-step *net*)

(format t "Danger: ~A~%" (get-value *danger*)) ;; 0.8
(format t "Run:    ~A~%" (get-value *run*))    ;; 0.2
```

------------------------------------------------------------------------

## Usage Patterns

### 1. Fuzzy Control

Use weighted AND/OR logic to encode human‑interpretable control rules.

### 2. Agent Behaviour Selection

Combine multiple soft signals to produce smooth behaviour transitions.

### 3. Recurrent Networks

Because connections are explicit and clamped, recurrent loops are safe and useful for dynamic systems.

### 4. Soft Logic for Games

Blend multiple influences (distance, light, noise, health, etc.) into decisions without brittle boolean thresholds.

------------------------------------------------------------------------

## Notes on Update Semantics

`compute-step` evaluates gates *in sequence*, not strictly
synchronously.

For stable multi-layer propagation, call:

``` lisp
(loop repeat 5 do (compute-step *c*))
```

For most use‑cases (biological modelling, AI behaviour trees, continuous logic), this is desirable.

------------------------------------------------------------------------

## License

MIT.

------------------------------------------------------------------------

## Maintainer

Joshua Boulton
jjrboulton.github.io