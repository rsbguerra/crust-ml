# Tests

This folder contains good and bad tests. Inside each folder you will find a category of tests.

The tests have the following structure:

```
.
├── tests
│   ├── good
│   │   ├── Comments
│   │   |   ├── 01_comments.rs
│   │   |   ├── 01_comments.out
│   │   |   ├── 02_comments.rs
│   │   |   ├── 02_comments.out
│   │   |   ├── 03_comments.rs
│   │   |   └── 03_comments.out
│   ├── bad
│   │   ├── Comments
│   │   |   ├── 01_comments.rs
│   │   |   ├── 02_comments.rs
│   │   |   └── 03_comments.rs
```

## Good Tests

A good test has two components, the code (in a .rs file) and the output that will be compared to the output of the code file (a .out file).


Example:

File `goodtest.rs`:

```Rust
fn main() 
{
    println!("This is a good test!");
}
```

File `goodtest.out`:

```
This is a good test!

```

Output:
```
This is a good test!

```

## Bad tests

A bad test is only composed by the code file (*.rs*). This a bad test passes if it produces an error error message.

Example:

File `badtest.rs`:

```Rust
/* 
    /* 
        /* 
        */ 
*/
```

Output:
```
File "./tests/bad/Comments/01_comment.rs", line 2, characters 7-8:

error:

  Syntatic error: invalid derivation.
```


## How to Add a New Test Category

To add a new test category you will first need to create a folder inside the `good` and `bad` directories. After this you only need to add a new test line in the `run-tests` script file.

Example:

File `run-test`

```Shell
sh ./run-test MyCategory
```

### Verbosity

The above example will only ouput log messages for the wrong tests, if you desire to have a full message to know exactly what is correct and what isn't you simple need to add the `-v` option at the end.

Example:

File `run-test`

```Shell
sh ./run-test MyCategory -v
```
