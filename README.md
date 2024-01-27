# Code

## Description

<details open>
  <summary>EN</summary>
  <p>The program checks whether a given set of words is a code, e.g. using the Sardinas-Paterson algorithm. The input set is given by a regular expression without asterisks.</p>
</details>

<details>
  <summary>PL</summary>
  <p>Program sprawdza, czy dany zbiór słów jest kodem, np. za pomocą algorytmu Sardinasa-Patersona. Zbiór na wejściu jest zadany za pomocą wyrażenia regularnego bez gwiazdek.</p>
</details>

## Usage

In order to run the program, you need to have Haskell installed. Then, you can run the program with the following command (you need to be in the `code` directory):

```bash
runghc ./Main.hs <input>
```

where `<input>` is a regular expression without asterisks. For example:

```bash
runghc ./Main.hs "0|10|110|111"
runghc ./Main.hs "0|10|010|101"
runghc ./Main.hs "car|pet|carpet"
```
