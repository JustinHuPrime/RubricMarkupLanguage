# Rubric Markup Language

![GitHub](https://img.shields.io/github/license/JustinHuPrime/RubricMarkupLanguage)

Are you a TA whose professor gives you long, long marking rubrics with complicated scoring calculations? Do you wish you could speed up your grading and turn the rubrics your professor gives you into fill-in-the-blank smart forms? The Rubric Markup Langauge allows you to turn any plain text file into a smart form.

## Installing

RML is distributed as a Racket program. You will need Racket to run this program.

## Invoking

RML is invoked with `rml.rkt [options...] input-filename`. The following options are accepted:

* `-o <output-filename>`: write the rendered RML to the specified file
* `--debug`: print all of the defined values after producing the rendered RML
* `--help`, `-h`: print a summary of command line options and exit (does not expect the `input-filename` to be given)
* `--`: consider all remaining arguments file names and not options

The `input-filename` is the name of an RML file to use.

## Syntax

RML is a functional style smart-form language. The language can ask for user input, transform those inputs in a limited way, and print out a filled-out form using the user input. RML has one scope and is functional style, meaning that there is a global environment into which identifiers may be given values, but an identifier that is already assigned a value may never get a new value.

In RML, there are four types of values:

* booleans, which store either true or false
* numbers, which store real-valued numeric data
* strings, which store sequences of characters
* grades, which are a number out of some maximum grade

An RML file consists of two sections: the header and the body. The header consists of any number of blank lines or line starting with a single dollar sign, and gets input from the user, specifies configuration options, and may also store values in variables. The body consists of plain text with the ability to interpolate expressions using `${...}`

The example.rml file is included below.

```rml
$# this is a comment

$input grade // 3 prompt "Question 1 grade (out of 3) " to q1grade
$input grade // 3 to q2grade if q1grade > 0 // 3 else 0 // 3

$let total = q1grade + q2grade

Your grade: ${total}

Question 1: ${q1grade}
Question 2: ${q2grade}
```

### Header Section

The header consists of either comment lines (lines starting with `$#`), blank lines (lines with only whitespace), and statements (lines starting with `$`). If any valid body line is encountered, the header stops right before that valid header line. Valid body lines include body lines starting with `$$`.

When a statement is encountered, it is evaluated, using the semantics below.

#### Config Statements

`$ config <option-name> <value>`

A config statement sets some configuration option to some value (which may be an expression to be evaluated). The following `option-name`s are accepted:

* `newline_collapse`: expects a boolean, starts out as true. Specifies whether adjacent blank lines in the output should be collapsed into a single blank line.
* `number_print_digits`: expects a positive integer, starts out as 2. Specifies the maximum precision when converting numbers to strings or printed text.
* `print_true_string`: expects a string, starts out as "true". Specifies the string to use when converting the boolean value true to a string or printed text.
* `print_false_string`: expects a string, starts out as "false". Same as `print_true_string` but for the boolean value false.
* `print_out_of_string`: expects a string, starts out as " out of ". Separator to put between the achieved points and maximum points when converting a grade to a string or printed text.

#### Input Statements

`$ input <type-spec> [prompt <prompt-expr>] to <destination-name> [if <condition> else <alternative>]`

An input statement asks for user input, and expects the input to be of the type specified in the `type-spec`. If the prompt is supplied, then the `prompt-expr` must evaluate to a string. The `destination-name` must be an identifier to save the user input to. If the conditional input is given, then the input will happen only if `condition` (which must evaluate to a boolean in all cases) is true, and if `condition` is false, the `alternative` (which does not have to satisfy the `type-spec`) is saved to the `destination-name` instead.

RML is a functional-style language, so it is not possible to overwrite existing identifiers with new values.

A `type-spec` may be:

* `number`
* `boolean`
* `string`
* `grade // <out-of>`

If the type-spec specifies a grade, the out-of value must be an expression that evaluates to a positive number, but does not necessarily have to be a whole number.

#### Let Statements

`$ let <name> = <value>`

Assigns the result of evaluating `value` to `name`. Like an input statement, this may not overwrite an existing identifier.

### Body Section

The body consists of any number of lines containig any amount of text. This text is outputted verbatim with the following exceptions: the dollar sign (`$`) is considered a special character, so any occurrence of one dollar sign must be as part of the following sequences:

* `$$`: prints a single dollar sign instead
* `${<expression>}`: inserts the stringified representation of the expression into output in place of the entire interpolation sequence
