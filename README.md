# tryton2ew

`tryton2ew` is an experimental Haskell application for analysing a
Tryton application tree and translating its structure into artifacts that can
be used to build an EasyWordy/FUDD **Wapp**.

The project explores a larger question:

> How much of an existing, complex business application can be mechanically
> reconstructed as a FUDD Wapp by analysing the application's declarative
> definitions, data model, navigation structure, views, localisation files,
> and implementation code?

The initial target has been the Tryton ecosystem, including the Tryton-based
GNU Health application. Tryton is useful for this experiment because a large
part of an application's structure is represented explicitly through module
configuration files, XML view and menu definitions, Python model classes, and
localisation catalogs.

`tryton2ew` scans those artifacts, reconstructs an intermediate representation
of the Tryton application, and generates parts of an EasyWordy application:
navigation, dynamic components and routes, database access code, native
function dispatching, and related integration material.

The project is currently a **research/prototyping tool**, not a general-purpose
Tryton migration utility.

---

## What the experiment is trying to achieve

A mature Tryton application contains useful information at several levels:

- module declarations and dependencies;
- menus and navigation hierarchy;
- actions and model/view associations;
- form and tree definitions;
- Python model declarations;
- database field definitions;
- localisation catalogs;
- application relationships and references;
- user-interface semantics.

A conventional port would reproduce these structures manually in the target
application.

`tryton2ew` instead investigates whether they can be treated as a source model:

```text
                  Tryton application
                         |
          +--------------+--------------+
          |              |              |
        .cfg            XML           Python
          |              |              |
          |           views/menu       models
          |           actions/etc.     fields
          |              |              |
          +--------------+--------------+
                         |
                      PO/POT
                         |
                         v
              +---------------------+
              | Tryton application  |
              | intermediate model  |
              +---------------------+
                         |
             +-----------+-----------+
             |                       |
             v                       v
       EasyWordy / Fuddle       Haskell backend
       Wapp artifacts           and DB operations
```

The aim is not source-to-source translation of Python into Haskell or XML into
Elm. Instead, the importer tries to recover the **application semantics
represented by those files** and regenerate corresponding constructs in the
FUDD/EasyWordy architecture.

---

## Current status

The repository should be considered experimental.

The current implementation can already:

- recursively scan a Tryton source tree;
- locate Tryton module configuration files;
- associate XML, Python, and localisation files with their modules;
- read module dependencies and XML entry points;
- parse Tryton menu definitions;
- reconstruct hierarchical menu trees;
- parse action-window/model definitions;
- parse Tryton UI view files;
- distinguish several Tryton view categories;
- parse Python source and recover selected model structures;
- derive SQL-oriented model descriptions;
- parse `.po` and `.pot` localisation data;
- consolidate multiple Tryton modules into one application representation;
- generate EasyWordy/Fuddle navigation;
- generate Wapp dynamic component modules;
- generate dynamic route definitions;
- derive Haskell-side SQL fetch/insert operations;
- generate Haskell DB modules and a native-function dispatcher;
- emit endpoint/function-definition material for integration into a Wapp.

The conversion is **not yet complete or generic**. In particular, there are
still assumptions originating from the GNU Health prototype, incomplete view
and behavioural mappings, and parts of the generator that intentionally
produce scaffolding rather than a complete replacement application.

---

## Architecture

The conversion is organized as a pipeline:

```text
 filesystem scan
      |
      v
 module discovery
      |
      v
 Tryton parsing
      |
      v
 module-level representation
      |
      v
 application consolidation
      |
      v
 EasyWordy generation
```

### 1. Filesystem discovery

`Commands.Importer` recursively scans the requested source directory.

The currently recognized file types are:

```text
.cfg
.xml
.py
.po
.pot
```

Other files are ignored by the discovery pass.

Tryton `.cfg` files are particularly important because they establish module
boundaries and expose information such as dependencies and XML resources.

---

## 2. Tryton module reconstruction

`Tryton.Load` converts the discovered files into `ModuleSrcTT` values.

A module records information such as:

```haskell
data ModuleSrcTT = ModuleSrcTT
  { nameMT        :: String
  , locationMT    :: Text
  , dependsMT     :: [String]
  , targetMT      :: Maybe FilePath
  , dataSpecMT    :: [FilePath]
  , supportSpecMT :: [FilePath]
  , localesMT     :: [FilePath]
  , viewsMT       :: [FilePath]
  , miscXmlMT     :: [FilePath]
  , logicMT       :: [FilePath]
  }
```

Files found during the directory scan are associated with the module whose
directory contains them.

This gives the importer a module-aware representation instead of a flat
collection of source files.

---

## 3. Parsing the application

Each discovered module is expanded into a `FullModuleTT`.

Conceptually, it contains:

```text
Tryton module
  |
  +-- module metadata
  +-- menu definitions
  +-- action windows
  +-- XML definitions
  +-- locales
  +-- view definitions
  +-- Python logic/model information
  +-- SQL model definitions
```

The relevant parser modules include:

```text
Parsing.Xml
Parsing.Python
Parsing.Pot
```

### XML

Tryton XML is used to recover information including:

- menu items;
- menu parent/child relationships;
- model instances;
- action windows;
- view references;
- tree views;
- form views;
- list definitions;
- graph definitions;
- board definitions.

`Tryton.Process` subsequently consolidates these definitions and builds the
application menu hierarchy.

### Python

`Parsing.Python` uses the Haskell `language-python` package to inspect Python
source.

The importer is interested primarily in application structure rather than
attempting arbitrary Python-to-Haskell translation.

Recovered model information is used by later generation stages to infer
database structures and operations.

### Locales

`.po` and `.pot` files are parsed and grouped by module and locale.

Localisation information can then be associated with generated menus,
components, labels, and fields.

---

## 4. Application intermediate representation

Parsed modules are consolidated into a `TrytonApp`:

```haskell
data TrytonApp = TrytonApp
  { modulesTA         :: [FullModuleTT]
  , localesTA         :: LocaleForModule
  , menuTreeTA        :: [MenuItem]
  , instancesByKindTA :: Map Text [ModelInstance]
  }
```

This is an important architectural boundary in the experiment.

The intended direction is:

```text
Tryton-specific parsers
        |
        v
     TrytonApp
        |
        v
target-specific generators
```

Keeping those two sides separate makes it possible to improve the Tryton
analysis independently from the EasyWordy generation logic.

It also creates the possibility of eventually introducing a more generic
application intermediate representation between them.

---

## 5. EasyWordy/Wapp generation

`Generation.EasyWordy` is currently the primary generation coordinator.

It combines:

- the consolidated menu tree;
- model/action definitions;
- parsed Tryton models;
- view definitions;
- SQL model descriptions;
- localisation data.

From these it constructs EasyWordy components and supporting Wapp artifacts.

The current generator produces artifacts including:

```text
wapp/
  Protected/
    LeftMenuNav.elm

  Components/
    Frames/
      <generated components>.elm

  DynRoutes.elm

HsLib/
  DB/
    <generated database modules>.hs

  DB.hs
  FctDispatcher.hs
  nativeLib.txt

menuTree.txt
yamlEntries.txt
```

The exact set of files will evolve with the experiment.

---

## Generated navigation

The reconstructed Tryton menu hierarchy is converted into EasyWordy navigation
definitions.

Tryton menus with children become composed navigation items, while leaves
become navigable Wapp components.

The generated navigation currently targets:

```text
wapp/Protected/LeftMenuNav.elm
```

A diagnostic representation of the reconstructed Tryton tree is also written
to:

```text
menuTree.txt
```

This file is useful when validating whether the importer has correctly
understood the source application's navigation structure.

---

## Generated Wapp components

Menu/action definitions are converted into component descriptions, which are
then rendered as Elm/Fuddle modules under:

```text
wapp/Components/Frames/
```

The generated components use the EasyWordy/Fuddle dynamic-function
architecture, including concepts such as:

- `default`;
- `continuations`;
- dynamic invocation;
- native parameters;
- HTMX interaction;
- EasyWordy function routing.

Component generation is currently one of the main experimental areas of the
project.

Not every Tryton view or interaction has a complete EasyWordy equivalent yet.

---

## Dynamic routes

The generator builds:

```text
wapp/DynRoutes.elm
```

from the generated component set.

Each generated component is registered with the dynamic function router,
connecting a Tryton-derived application function or menu entry to its
EasyWordy implementation.

This avoids requiring a manually maintained static route definition for every
imported Tryton screen.

---

## Database and native-function generation

Python model definitions are used to derive SQL-oriented model descriptions.

`Generation.Sql` and `Generation.HsLib` then generate parts of the
server-side Wapp integration, including:

```text
HsLib/DB/*.hs
HsLib/DB.hs
HsLib/FctDispatcher.hs
HsLib/nativeLib.txt
```

The generated Haskell code includes database operations derived from the
reconstructed Tryton models, currently including generated fetch and insert
operations.

`FctDispatcher.hs` bridges EasyWordy native-function invocation with the
generated database functions.

This part of the experiment demonstrates an important property of the
approach: recovering the source application's model metadata can generate not
only UI structure but also significant parts of the target application's
backend plumbing.

---

## Repository structure

The main source areas are:

```text
app/
  Main.hs

src/
  Commands/
  DB/
  Generation/
  HttpSup/
  Options/
  Parsing/
  Tryton/

  MainLogic.hs
```

Important modules include:

| Module | Responsibility |
| --- | --- |
| `Commands.Importer` | Top-level import/conversion workflow |
| `Tryton.Load` | Module discovery and loading |
| `Tryton.Process` | Consolidation and Tryton structure processing |
| `Tryton.Types` | Tryton intermediate data model |
| `Parsing.Xml` | Tryton XML parsing |
| `Parsing.Python` | Python/model analysis |
| `Parsing.Pot` | `.po`/`.pot` localisation parsing |
| `Generation.EasyWordy` | Main Wapp generation coordinator |
| `Generation.Fuddle` | Fuddle/Elm source generation |
| `Generation.Elm` | Elm-oriented generation helpers |
| `Generation.Views` | View conversion support |
| `Generation.Sql` | SQL model/schema generation |
| `Generation.SqlAst` | SQL representation |
| `Generation.HsLib` | Generated Haskell/native DB layer |
| `Generation.DataPrep` | Database bootstrap/data preparation |
| `Generation.EwTypes` | Internal EasyWordy generation types |
| `Generation.Svg` | SVG-related generation support |
| `Generation.Utils` | Generator utilities |

---

## Building

The project is a Stack/Hpack Haskell project.

The current package version is:

```text
0.1.0.0
```

and the Stack configuration uses the Stackage `lts-22.44` snapshot.

Build with:

```bash
stack build
```

### Local `ConfigFile` dependency

The current `stack.yaml` refers to a local package:

```yaml
extra-deps:
- ../../LocalPkgs/configfile
```

A fresh standalone clone therefore requires that dependency to exist at the
expected location, or that `stack.yaml` be adjusted to point to an equivalent
available `ConfigFile` package.

This is one of the repository's current development-environment assumptions.

---

## Configuration

The executable currently reads a YAML configuration file before dispatching
commands.

Unless overridden, the default path is:

```text
~/.config/genapp.yaml
```

A minimal development configuration can look like:

```yaml
debug: null
primaryLocale: en
```

A different configuration can be supplied with:

```bash
stack exec tryton2ew -- \
  --config /path/to/config.yaml \
  import ...
```

Some internal names and environment variables still retain the earlier
`extractor` naming used during development.

---

## Usage

The principal command is:

```bash
stack exec tryton2ew -- import SOURCE_DIR DEST_DIR
```

For example:

```bash
stack exec tryton2ew -- \
  import \
  /srv/src/tryton/modules \
  /srv/wapps/gnuhealth
```

`SOURCE_DIR` should point to the root of the Tryton application or module tree
to inspect.

`DEST_DIR` is the target EasyWordy/Wapp tree.

### Important

The generator currently writes to fixed paths such as:

```text
DEST_DIR/wapp/Protected/
DEST_DIR/wapp/Components/Frames/
DEST_DIR/HsLib/DB/
```

The intended use is therefore against an already prepared Wapp/application
tree. The current generator should not be treated as a complete Wapp project
bootstrapper.

---

## Import options

The CLI currently exposes:

```text
--schema,   -s
--dataprep, -p
--noapp
--nopot
--nopy
```

The current active importer uses:

```text
--nopot
```

to suppress most locale loading while retaining the base module POT
definitions needed by the conversion, and:

```text
--nopy
```

to skip Python/application-logic parsing.

The other switches originated in the earlier importer pipeline and are still
present in the command-line interface, but the current module-oriented import
path does not yet apply all of them. They should therefore be regarded as
work-in-progress controls until the newer pipeline incorporates their
corresponding generation stages.

---

## Example conversion flow

Given a Tryton tree conceptually containing:

```text
modules/
  party/
    tryton.cfg
    party.py
    locale/
      party.pot
      fr.po
    view/
      party_form.xml
      party_tree.xml
    party_view.xml

  company/
    tryton.cfg
    company.py
    ...
```

`tryton2ew` performs approximately:

```text
1. recursively discover supported files
2. find each Tryton module's .cfg
3. associate files with their module
4. read module dependency and XML configuration
5. parse main XML definitions
6. parse individual form/tree/etc. views
7. parse localisation catalogs
8. inspect Python model declarations
9. derive SQL model descriptions
10. consolidate module menus and model instances
11. build one TrytonApp
12. translate the menu tree into EasyWordy navigation
13. construct generated Wapp components
14. construct dynamic routes
15. generate Haskell DB/native glue
16. write generated artifacts into the target Wapp tree
```

---

## Why Tryton?

Tryton is not merely a collection of Python screens.

A substantial amount of application meaning is represented declaratively
through configuration, XML, model metadata, and localisation resources. This
makes it an interesting source system for automated application
reconstruction.

For the FUDD ecosystem, the broader experiment is relevant beyond Tryton.

A sufficiently capable application scanner could potentially turn an existing
software system into:

```text
source application
      |
      v
structural / semantic model
      |
      +-------------------+
      |                   |
      v                   v
generated Wapp       knowledge model
      |                   |
      v                   v
human refinement     AI-assisted refinement
```

Rather than asking an AI system to recreate a complex application based only
on screenshots or prose, tools such as `tryton2ew` can first extract a
structured representation of what already exists.

That structured information can then become input to deterministic generators,
human developers, or AI-assisted software-development workflows.

---

## Experimental limitations

The current implementation has several important limitations.

### It is not a full Tryton interpreter

The Python parser recovers selected structures relevant to conversion. It does
not reproduce arbitrary Python execution semantics.

Business logic implemented procedurally in Python may therefore require manual
implementation or a later AI-assisted conversion phase.

### View support is incomplete

Tryton contains multiple view and interaction mechanisms. The repository has
representations for tree, form, list, graph, board and related definitions,
but conversion coverage is not yet complete.

Some generated components are therefore structural scaffolding rather than
complete behavioural equivalents.

### Backend generation is still prototype-specific

Some generated module names, package names, native-function identifiers, and
imports currently contain GNU Health-specific assumptions, including
`Wapp.Apps.GnuHealth` namespaces.

These need to become target-application configuration before `tryton2ew` can
be considered a generic Tryton converter.

### Generated application behaviour requires validation

Successful parsing does not imply semantic equivalence.

Generated applications must be checked for:

- menu completeness;
- model relationships;
- field semantics;
- permissions;
- validation rules;
- workflows;
- defaults;
- computed fields;
- action behaviour;
- transaction semantics;
- localisation correctness;
- database constraints.

### Output generation assumes a target scaffold

The current code writes generated files into expected Wapp/Haskell paths
rather than constructing an entire standalone application repository.

---

## Development direction

The experiment naturally separates into several increasingly ambitious
stages.

### Stage 1 — structural extraction

Recover as much deterministic information as possible from:

```text
Tryton configuration
XML
Python ASTs
locales
module relationships
```

This is the current foundation.

### Stage 2 — normalized application model

Reduce Tryton-specific structures into a cleaner intermediate representation
covering concepts such as:

```text
Entity
Field
Relation
View
Action
Menu
Workflow
Validation
Locale
Permission
```

This would reduce coupling between the Tryton parsers and EasyWordy
generators.

### Stage 3 — deterministic Wapp generation

Generate the portions for which translation rules are reliable:

```text
navigation
CRUD views
routing
SQL
native DB operations
locales
forms
tables
```

### Stage 4 — assisted semantic conversion

Use the extracted application model together with original Python
implementation fragments as structured input for AI-assisted generation of
business logic that cannot be translated mechanically.

### Stage 5 — validation

Compare source and generated applications structurally and behaviourally.

Potential validation artifacts include:

```text
module coverage
menu coverage
model coverage
field coverage
view coverage
action coverage
translation coverage
unsupported construct reports
```

This would make conversion measurable rather than relying on visual inspection
alone.

---

## Design principle

`tryton2ew` deliberately favors:

```text
parse
  -> understand
  -> normalize
  -> generate
```

over:

```text
copy
  -> regex-rewrite
  -> hope
```

The long-term value of the project is therefore not just its current generated
files.

It is the exploration of a reusable methodology for **mechanically acquiring
the structure of an existing application and transforming that knowledge into
a native FUDD application representation**.

---

## Project status

`tryton2ew` is an experimental FUDD project.

Expect:

- incomplete mappings;
- prototype-specific assumptions;
- evolving intermediate types;
- generated code requiring review;
- breaking changes as the Wapp model evolves.

It is most useful today as:

1. a Tryton application analysis tool;
2. a prototype application-reconstruction pipeline;
3. a generator for portions of an EasyWordy Wapp;
4. a test bed for larger automated software-migration techniques.

---

## License

BSD-3-Clause. See `LICENSE`.