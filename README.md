# FortranMatMult – Matrix Multiplication in Fortran

**FortranMatMult** is a lightweight Fortran program for multiplying matrices of arbitrary size. It was developed as part of my engineering informatics training at the University DUE and reflects foundational knowledge in numerical methods and scientific computing.

> 🧠 Matrix multiplication is a core operation in many fields, including the Finite Element Method (FEM) and Machine Learning (AI).

---

## 📋 Features

- Supports multiplication of matrices with compatible dimensions  
- Reads input from structured text files (`input.txt`, `mmult.in`)  
- Outputs results to screen or file (`mmult.out`)  
- Modular structure with separate main and library files (`MatMultMain.f90`, `MatMultlib.f90`)  

---

📁 ## Repository Structure

├── MatMultMain.f90       # Main program
├── MatMultlib.f90        # Matrix multiplication subroutines
├── input.txt             # Sample input file
├── mmult.in              # Alternative input format
├── mmult.out             # Output file
├── MatMult.cbp           # Code::Blocks project file


## 📂 Input Format

Input files specify the number of rows and columns for each matrix, followed by the matrix values in row-major order. Example input files (`input.txt`, `mmult.in`) are included in the repository.

## 🧪 Applications

Matrix multiplication is a fundamental operation in:

- **Finite Element Method (FEM)** – for assembling system matrices and solving linear systems  
- **Scientific Computing** – where linear algebra is at the core of simulation, modeling, and data analysis
- **Machine Learning (AI)** – for operations in neural networks, gradient calculations, and feature transformations  
- **Engineering Education** – for understanding numerical algorithms and practicing Fortran programming

## 🚀 Getting Started

### 🔧 Compilation

Use a Fortran compiler like `gfortran`:

```bash
gfortran -o matmult MatMultMain.f90 MatMultlib.f90
./matmult
