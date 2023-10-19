# Todo Scanner

```
 /$$$$$$$$        /$$$$$$$  /$$$$$$$         /$$$$$$                      /$$   /$$                    
|__  $$__/       | $$__  $$| $$__  $$       /$$__  $$                    | $$$ | $$                    
   | $$  /$$$$$$ | $$  \ $$| $$  \ $$      | $$  \__/  /$$$$$$$  /$$$$$$ | $$$$| $$  /$$$$$$   /$$$$$$ 
   | $$ /$$__  $$| $$  | $$| $$  | $$      |  $$$$$$  /$$_____/ |____  $$| $$ $$ $$ /$$__  $$ /$$__  $$
   | $$| $$  \ $$| $$  | $$| $$  | $$       \____  $$| $$        /$$$$$$$| $$  $$$$| $$$$$$$$| $$  \__/
   | $$| $$  | $$| $$  | $$| $$  | $$       /$$  \ $$| $$       /$$__  $$| $$\  $$$| $$_____/| $$      
   | $$|  $$$$$$/| $$$$$$$/| $$$$$$$/      |  $$$$$$/|  $$$$$$$|  $$$$$$$| $$ \  $$|  $$$$$$$| $$      
   |__/ \______/ |_______/ |_______/        \______/  \_______/ \_______/|__/  \__/ \_______/|__/      
                                                                                                       
```

Todo Scanner is a command-line tool written in Haskell for searching for specific labels (e.g., "TODO," "FIXME," "XXX") in files within a specified directory with particular file extensions.

## Requirements

To use this tool, you need to have the following installed:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [cabal](https://www.haskell.org/cabal/)

## Installation

1. Download or clone this repository.
2. Execute the following commands to build and install the program:

```bash
   cd todo-scanner-directory
   cabal update
   cabal build
   cabal install
```

## Usage

```bash
todo-scanner [--dir DIRECTORY] [--label LABEL] [--exts EXTENSION]
```

Parameters:

1. `--dir DIRECTORY`: The directory to scan for files. (can be specified multiple times)
1. `--label LABEL`: Optional. A label to search for (can be specified multiple times).
1. `--exts EXTENSION`: Optional. File extensions to scan for (can be specified multiple times).

Examples:

* Search for TODOs in all files in the "my_project" directory:

```bash
todo-scanner --dir my_project
```

* Search for TODOs and FIXMEs in files with "js" and "php" extensions:

```bash
todo-scanner --dir my_project --label TODO --label FIXME --exts js --exts php
```

## How It Works

1. The program obtains input parameters from the command line using the "optparse-applicative" library.
1. It recursively searches for files in the specified directory with the specified file extensions.
1. For each found file, its content is read.
1. The program analyzes the content of each file, searching for matches with the specified labels.
1. Found matches are printed to the console, along with information about the line and file where they were found.
1. All found matches are highlighted in green, and other lines are highlighted in red.
1. After the search is complete, the program reports the total number of found matches.

## Example Output

If matches (e.g., "TODO") are found in files, the program's output may look like this:

```plaintext
------ --------------------------------------------------
Line: my_project/src/file.js
1     // TODO: Fix this issue
3     // TODO: Refactor this code
------ --------------------------------------------------
Found 2 TODOs

```

If no matches are found, the program reports:

```plaintext
        Found 0 TODOs
```

## Docker

`docker run -v $(pwd):/code --rm -it --entrypoint "/srv/todo-scaner" jtrw/todo-sacenr --dir=/code`

## Gitlab-Ci

```yaml
todo:
  stage: static-analizator
  image:
    name: jtrw/todo-scaner:latest
    entrypoint: [""] # disable entrypoint
  script:
    - /srv/todo-scaner --dir=./src
```



## Notes

* The program uses colored highlighting to make the output more readable. It may not work on all terminals.
* You can customize the default labels and file extensions by using your values.

This documentation provides an overview of the features and usage of Todo Scanner. For more information about capabilities and configuration, it is recommended to review the source code and Haskell documentation.

This tool is designed to make it easier for you to locate "TODO" labels in your project and help you focus on important tasks.