# Compiler for Language E

This is a project where I implemented a **compiler** for **Language E**, a small programming language inspired by C. The goal was to create a compiler that performs various stages of compilation and ultimately generates **RISC-V assembly code** that can be executed using `qemu`.

## 🛠️ Project Overview

The project focuses on building a complete compiler for **Language E**, from lexical analysis (lexer) to the generation of RISC-V code. The language is small and simple enough to be developed during the course, but it covers a wide range of fundamental concepts in compiler construction.

The compiler consists of:
- **Lexical analysis** (lexer)
- **Syntax analysis** (parser)
- **Intermediate code generation and passes**
- **RISC-V assembly code generation**

The generated assembly code is then assembled and executed using `qemu-riscv64`.

## 🏗️ Compiler Architecture

The compiler follows several stages:
1. **Lexical Analysis**: Tokenize the source code.
2. **Syntax Analysis**: Build the Abstract Syntax Tree (AST).
3. **Intermediate Compilation Passes**: Transformation of the code for optimization.
4. **Code Generation**: Generate **RISC-V assembly** code.

The intermediate steps include code transformations and optimizations.

## 🧪 Testing and Debugging

We have a testing infrastructure to ensure the correctness of the compiler:
- **Unit tests** for the lexer and parser.
- **End-to-end tests** to verify the entire compilation pipeline, from source code to RISC-V execution.

Test instructions are provided in the project documentation for easy debugging and validation.

## 📝 Project Breakdown

The project is divided into the following tasks:
- **Task 1**: Implementing the lexer (tokenizing the source code).
- **Task 2**: Writing the parser (building the Abstract Syntax Tree).
- **Task 3**: Developing intermediate code representations.
- **Task 4**: Implementing the RISC-V code generation.
- **Task 5**: Assembling and running the generated code using `qemu-riscv64`.

Results are available in tests/results.html
---

This project was completed as part of the **Compiler Design** course during my studies at CentraleSupélec in 2025.
