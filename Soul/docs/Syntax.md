# Soul Syntax Specification

## Overview
This document outlines the syntax and structural elements of the Soul Programming Language, designed for controlling real-world systems like IoT devices and robotics. Soul combines natural language-like syntax with strong static typing and polymorphic capabilities.

## Basic Syntax Rules
- **Case Sensitivity**: Soul is case-insensitive for keywords, but case-sensitive for identifiers.
- **Statements**: End with a semicolon `;` unless enclosed in braces `{}`.
- **Comments**: Single-line with `//`, multi-line with `/* */`.
- **Indentation**: Recommended 2 spaces for readability, not enforced.

## Data Types
- **Primitive Types**:
  - `Integer`: Whole numbers (e.g., `define count as Integer = 5;`)
  - `Float`: Decimal numbers (e.g., `define angle as Float = 45.5;`)
  - `Boolean`: True/False (e.g., `define active as Boolean = true;`)
  - `String`: Text data (e.g., `define name as String = "Robot1";`)
- **Composite Types**:
  - `Array`: Collection of same-type elements (e.g., `define readings as Array of Float = [1.2, 3.4];`)
  - `Stream`: Real-time data flow (e.g., `define dataFlow as Stream of Integer;`)

## Variable Declaration
- Format: `define <identifier> as <Type> = <value>;`
- Example: `define motorSpeed as Integer = 100;`

## Control Flow
- **If-Then-Else**:
  ```
  if condition then {
    action;
  } else {
    otherAction;
  }
  ```
- **Loop**:
  ```
  repeat until condition {
    action;
  }
  ```
- **Stream Loop**:
  ```
  stream data from source every <interval> do {
    process(data);
  }
  ```

## Object-Oriented Constructs
- **Entity (Class)**:
  ```
  entity Robot {
    property id as String;
    property position as Float = 0.0;
    action move to newPosition as Float {
      set position to newPosition;
      hardware sync;
    }
  }
  ```
- **Instantiation**: `new Robot with id "R1";`

## Hardware Interaction
- **Binding**: `bind motor to port1;`
- **Command**: `command motor move at speed 50;`
- **Sync**: `hardware sync;` to ensure hardware state matches software state.

## Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Comparison**: `equals`, `not equals`, `greater than`, `less than`
- **Physical**: `>>` (stream pipe), `~` (approximate equality)
- **Mechanical**: `@` (hardware bind), `^` (power level)
- **Stream**: `|` (parallel), `&` (merge)

## Functions
- Format:
  ```
  define action processData with input as Integer returns Boolean {
    if input greater than 0 then {
      return true;
    }
    return false;
  }
  ```

## NLP Commands
- Natural language commands are prefixed with `command`:
  - `command "move robot forward by 10 units";`
  - Internally translates to structured Soul code.

## Error Handling
- **Try-Catch**:
  ```
  try {
    hardware sync;
  } catch error {
    log "Sync failed: " + error;
  }
  ```

## Modules and Imports
- **Define Module**: `module IoTControl { /* code */ }`
- **Import**: `use module IoTControl;`

## Energy Optimization
- Command: `optimize power for <task> every <interval>;`
- Example: `optimize power for sensor read every 5s;`

## MindSpace API Integration
- Simulation: `mindspace simulate robot move to 10.0;`
- Test: `mindspace test stream dataFlow;`

This specification will evolve as Soul develops, incorporating feedback from prototypes and real-world tests.
