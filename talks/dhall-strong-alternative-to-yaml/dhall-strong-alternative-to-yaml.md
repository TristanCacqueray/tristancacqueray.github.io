% Dhall: A Strong Alternative to YAML
% Tristan de Cacqueray
% Februrary 18, 2021

# Overview

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">

If like me, you are tired of writting YAML files, this talk is for you.

- Why do we want an alternative?
- What is Dhall?
  - Let bindings
  - Imports
  - Functions
- How to use it?
- Food for thought

# Why do we want an alternative?

- Strong language safety:
  - Truncated files must be invalid.
  - No implicit convertion, `NO` != `false`.
- Better operators to manage repetition.
- Integrated string interpolation.
- Import file and URL.

# ![dhall](https://dhall-lang.org/img/dhall-large-logo.svg){ width=250px }

> a programmable configuration language that you can think of as: JSON + functions + types + imports

- Get the reference interpreter from the fedora packages:

```shell
$ sudo dnf install -y dhall dhall-json
```

- Simple scalar types:

  - Text: `"Hello, world!"`
  - Bool: `True`, `False`
  - Natural: `0`, `1`, `2`, …
  - Integer: …, `-2`, `-1`, `+0`, `+1`, `+2`, …
  - Double: `3.14159265`, `6.0221409e+23`

- Complex composite types:

  - Optional: `Some 1`, `None Natural`
  - List: `[1, 2, 3]`
  - Record: `{ x = 1.2, y = -2.5 }`
  - Union: `< A | C | G | T >.A`

- Example configuration:

```haskell
{ job =
  { name = "koji-build-x86_64"
  , run = "playbooks/rpmbuild.yaml"
  , vars = { arch = "x86_64", build = < scratch | final >.scratch }
  }
}
```

… corresponds to this YAML:

```bash
$ dhall-to-yaml --file ./job.dhall
```

```yaml
job:
  name: koji-build-x86_64
  run: playbooks/rpmbuild.yaml
  vars:
    arch: x86_64
    build: scratch
```

- Benefits:
  - Doesn't accept malformed input.
  - Can't produce malformed output.
  - Simpler standard than YAML.

# Let bindings

- Decompose expressions into sub expressions:

```haskell
let name = value

in expr
```

- Example usage:

```haskell
let job =
      let arch = "x86_64"

      let build = < scratch | final >.scratch

      in  { name = "koji-build-${arch}"
          , run = "playbooks/rpmbuild.yaml"
          , vars = { arch, build }
          }

in  { job }
```

- Benefits:
  - Large configurations are easier to manage.

# Imports

- Value can reference a path:

```haskell
-- ./koji-job.dhall file content:
let arch = "x86_64"

let build = < scratch | final >.scratch

in  { name = "koji-build-${arch}"
    , run = "playbooks/rpmbuild.yaml"
    , vars = { arch, build }
    }
```

- The previous example can be rewritten as:

```haskell
let job = ./koji-job.dhall

in { job }
```

- Benefits:
  - Configurations can be split.
  - Enables single source of truth.

# Functions

- Function definition:

```haskell
-- ./koji-job.dhall
\(build : < scratch | final >) ->
\(arch : Text) ->
  { name = "koji-build-${arch}"
  , run = "playbooks/rpmbuild.yaml"
  , vars = { arch, build }
  }
```

- Can be evaluated:

```bash
$ dhall-to-yaml <<< './koji-job.dhall (< scratch | final >.final) "x86_64"'
```

… corresponds to this YAML:

```yaml
name: koji-build-x86_64
run: playbooks/rpmbuild.yaml
vars:
  arch: x86_64
  build: final
```

- Benefits:
  - Don't repeat yourself.
  - Configuration re-use.
  - Treat the configuration as your code.

# Demo

- URL can be imported too:

```haskell
https://softwarefactory-project.io/cgit/software-factory/sf-infra/plain/Infra/OpenShift/deployWithEnv.dhall
  "service-name"
  "quay.io/software-factory/service-name:v1"
  8080
  "service-name.prod.psi.redhat.com"
  (toMap { option = "value", logLevel = "INFO" })
```

… corresponds to this YAML:

```bash
$ dhall-to-yaml --documents --file ./demo.dhall
```

```yaml
---
apiVersion: v1
kind: Route
metadata:
  name: service-name
spec:
  host: service-name.prod.psi.redhat.com
  path: /
  port:
    targetPort: 8080
  tls:
    insecureEdgeTerminationPolicy: Redirect
    termination: edge
  to:
    kind: Service
    name: service-name-service
    weight: 100
---
apiVersion: v1
kind: Service
metadata:
  name: service-name-service
spec:
  ports:
    - port: 80
      targetPort: 8080
  selector:
    app: service-name
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: service-name
spec:
  replicas: 1
  selector:
    matchLabels:
      app: service-name
  template:
    metadata:
      labels:
        app: service-name
      name: service-name
    spec:
      containers:
        - env:
            - name: logLevel
              value: INFO
            - name: option
              value: value
          image: quay.io/software-factory/service-name:v1
          name: service-name
          ports:
            - containerPort: 8080
```

- Benefits:
  - Dhall is general purpose.
  - It doesn't require Jinja or Go template tranformation.
  - It can be used for other configurations besides kubernetes.

# Usage

- Adopt Dhall in four steps:

  - First generate YAML files from Dhall expressions.
  - When satisfied, commit the Dhall files along with your changes.
  - Add a CI job to ensure the YAML files do not drift.
  - Remove the YAML and load Dhall directly.

- Dhall can also be used for:
  - Text (Containerfile)
  - Bash (script variables)
  - XML

# Food for thought

- Import supports semantic integrity checks.

- Dhall code can be tested with inlined assertion.

- Dhall diff can compare the semantic of two expressions.

- Dhall is total and not Turing complete:

  - configuration files should not permit arbitrary side effects.
  - configuration files should not enable excessive indirection or obfuscation.
  - configuration files should not crash or throw exceptions.

- Mature community and ecosystem: [awesome-dhall](https://github.com/dhall-lang/awesome-dhall)

- Checkout the [Language tour](https://docs.dhall-lang.org/tutorials/Language-Tour.html)

- Learn more at [https://dhall-lang.org](https://dhall-lang.org)

# Questions?

- Send them at [tdecacqu@redhat.com](mailto:tdecacqu@redhat.com)
- Slides are available at [https://tristancacqueray.github.io/talks/dhall-strong-alternative-to-yaml/](https://tristancacqueray.github.io/talks/dhall-strong-alternative-to-yaml/)
