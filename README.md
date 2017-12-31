# clution

## Table of Contents

* [Purpose](#Purpose)
* [Features](#Features)
* [Installation](#Installation)
* [Usage](#Usage)
  * [Build](#Build)
  * [Run](#Run)
  * [REPL](#REPL)
  * [clutex](#clutex)
* [clution-file](#clution-file)
* [asd clution](#asd-clution)
* [qlfiles](#qlfiles)
* [clu-directory](#clu-directory)
* [Rationale](#Rationale)

## Purpose

clution is an IDE package for developing Common Lisp(CL) projects.

It aims to do the following:

* Be a platform for CL projects to get started, quickly, and easily
* Provide templates for different types of project
* Streamline usage of local project development
* Create a consistent point of entry to a project

In achieving these goals, development of this project shall leverage and
incorporate support for as much existing technology as possible.
This includes, but is not limited to:

* ASDF
* Quicklisp
* Qlot
* Roswell
* SLIME
* Sly

## Features

At its core, a clution represents a project. That is, a piece of software.
Commonly in CL, what a project consists of is:

* ASDF Systems, developed as part of the project
* 3rdparty ASDF systems, external to the project (dependencies)
* ASDF systems comprising a test suite
* When using qlot, a qlfile specifying dependencies
* When building a script, a script file that is the entry point
* When building an application, often a separate file that defines a toplevel

What a clution does, is serve as a central hub in your project for all of these
components. More importantly, it does so in a way that is local to the project.

## Installation

Simply place [clution.el](clution.el) in your emacs load-path and load.

clution operates as a minor mode in emacs. Enable it via
`(clution-mode t)`

Once loaded, make sure to customize the following variables

* `clution-frontend` - defaults to `roswell`
* `clution-backend` - defaults to `sbcl`
* `clution-repl-style` - should automatically default to `sly` or `slime`

See their documentation strings for more information.

## Usage

Once enabled, clution will automatically activate when opening a .clu or .asd
file (see `clution-auto-open`).
Additionally, you may call `M-x clution-open` or `M-x clution-open-asd`, respectively.
At any time, you may close the current clution by invoking `M-x clution-close`.

### Build

A clution may be built by pressing `C-S-b`(`M-x clution-build`).
This will perform an ASDF compile operation on each system in the clution.

NOTE: While a REPL is active, builds will take place in the current REPL

### Run

You may run the currently selected system by pressing `C-<f5>`(`M-x clution-run`).
This will load the selected system and invoke its toplevel function.
This function defaults to `COMMON-LISP-USER::MAIN`.

An example function is provided here:

``` common-lisp
(in-package #:cl-user)

(defun main (&rest argv) ; roswell style &rest argv
  (format t "Hello, world!~%")
  0) ; return value is exit code for progress (0 if not an integer)
```

WARNING: Running while a repl is active will end the repl before running

### REPL

Running a REPL in clution is done by pressing `<f5>` (`M-x clution-repl`)
This will spin up a repl with the selected system loaded and ready.

### clutex

clutex is a special buffer & window which shows the currently loaded clution
in a neotree style tree.

Open and close it via `M-x clution-clutex-open` and `M-x clution-clutex-close`

At the top of this tree, there is the clution file itself.
If enabled for the clution, the [qlfile](#qlfiles) will be displayed directly
under the clution.
Afterwards, each system in the clution is visible, along with its components.

In the clutex buffer, you may visit any file by double-clicking or pressing
`Enter` on its node.

Here are some common operations in the clutex buffer:

* add system - `A` in the clutex buffer
* select system - `S` while system is under point
* remove system - `D` or `<delete>` while system is under point
* open file - `Enter` or double-click on system/file

The currently selected system is marked by a `<>` next to the file name

## clution file

At its simplest, a clution file lists the systems thare together form your
project.

It's possible to create a new clution file via `M-x clution-create-clution`
This will prompt for a file path, and create an empty clution file.
You may then open the clution `M-x clution-open`, and add systems to it by
pressing `A` while hovering over it in the [clutex](#clutex) buffer.
To remove a system, move the emacs point over the system name and press `D`.

However for many small projects, where only one system is relevant, see the
section on [asd clutions](#asd-clution).

## asd-clution

For convenience in single-system projects, clution is able to operate on single
asd files (`M-x clution-open-asd`).
When opening said file, clution will automatically generate a trivial clution
file & directories in its application data directory.
Otherwise, all normal operations, such as opening a repl, building, and
[clutex](#clutex) are available.

When a [qlfile](#qlfiles) is present in the same
directory as the asd, clution will use that file.

## qlfiles

[qlot qlfiles](https://github.com/fukamachi/qlot) are supported, though with
slightly different behavior from qlot.
When a clution specifies a qlfile, the operation `M-x clution-qlfile-sync` will
synchronize clutex's libraries with the qfile. By default, the packages are
stored in the clution's [clu-directory](#clu-directory).
When performing any operation on a clution, all packages in qlfile packages
will be visible to ASDF.

When naming conflicts occur, priority is given to clution-local systems over
qlfile systems, followed by any user-configured systems.

## clu-directory

clution stores project-local files, such as compiled fasls,
[qlfile packages](#qlfiles), and generated scripts used for building, in its clu
directory.
By default, this directory is named `.clu`, and is in the same directory as the
clution file itself.

## Rationale
In the current Common Lisp ecosystem, there exist fantastic tools that allow
developers to work and collaborate like never before.

Starting from the bottom, we have ASDF, serving as the de-facto build system
and system specification tool. At its most basic, ASDF allows a developer to
sanely structure their project across multiple files, specify dependencies
between those files, and perhaps more importantly, specify dependencies on
other systems.
With this tool, a developer is able to have a consistent, declarative
description of their project. When used correctly, this provides developers,
both new and old, with a consistent (across projects) way to form a good
mental model for a CL project.

Next, we have Quicklisp. With Quicklisp, we've a shared repository of such
systems. This provides a way for developers to trivially use established
libraries and tools in the CL ecosystem.
Developers are thus able to have a consistent way in which dependencies are
found, installed, and "linked" to a project.

And finally, (for the purposes of this segment) we have qlot.
With qlot, developers have the ability to integrate Quicklisp in a local
project. qlot is a solution to the problem of Quicklisp's "global"
installation behavior. Quicklisp installs libraries in a way that is global
to the user of an operating system (O.S.), or possibly, global to the O.S.
as a whole.
qlot thus provides a consistent way to allow developers to configure project
dependencies in a way that is project-local.

Each of these tools fulfills a different need in a developer's context. More
importantly they serve as a consistent way to do so. None of these tools are
strictly necessary in any development scenario. One could create a
complicated project, without a single bit of ASDF. But such a project would
have to use improvised, inconsistent techniques devised to solve the very
problems ASDF is made to address.
Likewise, Quicklisp need not be used, and project dependencies obtained and
installed by scouring the web and forums.
What these tools do is provide consistent solutions to common problems.
