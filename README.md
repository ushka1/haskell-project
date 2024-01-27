# Uniquely Decodable Code

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

To execute the program, ensure that [Haskell](https://www.haskell.org/) is installed on your system. Afterward, navigate to the `code` directory and initiate the program with:

```bash
cd code
runghc ./Main.hs <regex>
```

where `<regex>` is a regular expression without asterisks. For example:

```bash
runghc ./Main.hs "0|10|110|111" # True
runghc ./Main.hs "0|10|010|101" # False
runghc ./Main.hs "0|10|110|1110|11110" # True
runghc ./Main.hs "1|011|01110|1110|10011" # False
```

## Tests

In order to run the tests, navigate to the `code` directory and execute the following commands:

```bash
cd code
runghc ./RegexToWordSetTest.hs
runghc ./UniquelyDecodableTest.hs
```

## Limitations

Supported regex operators:

- `ab|cd` (or)
- `(abc)` (group)
- `[abc]` (character class)
- `[a-z]` (character class with range)
- `[a-zA-Z]` (character class with multiple ranges)

Currently not supported operators:

- `a?` (optional)
- `a{m}` (quantifier)
- `a{m,}` (quantifier)
- `a{m,n}` (quantifier)
