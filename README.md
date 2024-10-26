# TensorRight: Artifact Evaluation

This repo is the artifact associated with the POPL 2025 submission "TensorRight: Automated Verification of Tensor Graph Rewrites".

## List of Claims

### Claim 1: Expressiveness of TensorRight DSL

We compare the expressiveness of TensorRight with two other automatic tensor rewrite rules engines, [TASO](https://dl.acm.org/doi/10.1145/3341301.3359630) and [PET](https://www.usenix.org/system/files/osdi21-wang-haojie.pdf), across all the 175 rules. This corresponds to Section 7.1 and Table 1 in the paper.

For TensorRight, we can express 121 rules out of 175, and we implemented 118 of those in our DSL, as present in [`rules/*/Main.hs`](./rules/). Meanwhile, for TASO and PET, the number of rules they can represent was calculated based on the operators they support, as seen in their respective papers.

### Claim 2: Verification Capabilities of TensorRight

Out of the 121 rules that we can express in our DSL, we implemented 118 rules. We check how many rules TensorRight can verify in the unbounded setting. We support boolean, integer, and real-valued tensors in our DSL, and the rules are verified for all valid tensor types for that rule. We also measure the time taken to verify the rules in the unbounded setting (timeout of 10s for each tensor type). This corresponds to Section 7.2 and Figure 7 in the paper.

Out of the 118 rules we implemented, we are able to verify 114 rules. Out of the 4 rules that we cannot verify, 3 rules timed out, while 1 was proven incorrect (discussed in [Claim 3.1](#claim-31-maxmintoclamp-rule)).

### Claim 3: How TensorRight can be used to aid compiler developers in rapid developement

We showcase this claim using two case studies in Section 7.3.

#### Claim 3.1: `MaxMinToClampRule`

The `MaxMinToClamp` rule (implemented as [clamp/rule02](rules/clamp/Main.hs) and [maxMinToClampBefore](rules/maxMinToClampBefore/Main.hs)) is proven incorrect, since TensorRight returns a counterexample for the same. We modify the rule by adding a required precondition, as implemented in [maxMinToClampAfter](rules/maxMinToClampAfter/Main.hs), and show that the rule is verified.

#### Claim 3.2: Generalizing Rewrite Rules

We implement the `FoldConvInputPad` rule as [conv/rule00](rules/conv/Main.hs). This rule is very restrictive, since it contains some preconditions. We show that TensorRight can prove a more general version of this rule, as implemented in [foldConvInputPadGeneral](rules/generalize/Main.hs).

## Download, Installation and Sanity Testing

### Hardware Requirements

To use this artifact, you will need a x86-64 or a aarch64 machine capable of running Docker with at least 16GB of RAM.
The docker image would be about 11GB in size.
The artifact has not been tested on on legacy CPUs that do not support modern ISAs including AVX/AVX2.

We have tested the docker image on a physical machine running Ubuntu 22.04 LTS with an Intel(R) Core(TM) i7-13700H processor and 32 GB of RAM and a MacBook Air with Apple M2 Chip and 16GB or RAM.
The results you obtain may also vary from ours depending on your hardware and software configuration.

### Download and Installation

To use this artifact, you will need to install Docker on your machine.

#### Install Docker

These instructions have been tested on Ubuntu 22.04 LTS and are based on the official Docker [documentation](https://docs.docker.com/engine/install/ubuntu/).

```bash
## Add Docker's official GPG key:
sudo apt update
sudo apt -y install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

## Add the repository to Apt sources:
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt update

## Install Docker
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

## Add the current user to the docker group
sudo usermod -aG docker $USER

## Log out and log back again to apply the changes
## Check if Docker is installed correctly
# docker --version
# docker run hello-world
```

#### Build Docker Image or Pull from DockerHub

**Expected Time**: 10-12 minutes

##### Pull from DockerHub

```bash
docker pull lsrcz/tensor-right:latest
```

This command will pull the `lsrcz/tensor-right` image from DockerHub.
The docker image is built with the [Dockerfile](Dockerfile) present in this repo.
The docker image is based on Ubuntu 22.04 LTS and contains all the dependencies required to run the benchmarks.

##### Build from Source

To build the docker image, run the following command in the root directory of the artifact:

```bash
docker build -t tr .
```

This command will build the docker image named `tr` using the `Dockerfile` present in the root directory of the artifact.

#### Run Docker Container

To run the docker container, run the following command in the root directory of the artifact.

If you pulled from DockerHub:

```bash
docker run --rm -v "$(pwd)/plot:/home/tr/plot" -it lsrcz/tensor-right:latest /bin/bash
```

If you built the image from source code:

```bash
docker run --rm -v "$(pwd)/plot:/home/tr/plot" -it tr /bin/bash
```

You will be dropped into a bash shell inside the docker container, in the `/home/tr/` directory.
The `plot` directory in the host machine is mounted to the `/home/tr/plot/` directory in the docker container.
This is where the timing plot generated will be saved.
The Makefile in this directory contains the commands to run the benchmarks and generate the timing plot.
Details on how to run the benchmarks and generate the timing plot are provided in the next section.

### Sanity Testing

To test if the setup correctly, run the following command in the docker container:

```bash
stack exec rules-mul | grep -i fail || echo "Sanity Test passed"
```

This command will run the `rules-mul` executable, which verifies proper installation of the four important components: `z3`, `cvc5`, `stack` and `grisette`.
If the setup is correct, you should see the `Sanity Test passed` message printed on the terminal and no error messages.

## Evaluation Instructions

### Claim 2: Verification Capabilties and Statistics

**Expected Time**: 3-4 minutes

Script for this step:

```bash
make verify && make plot
```

`make verify` tries to verify all the 118 implemented rewrite rules, while logging the output in `plot/result.txt` (and also on stdout). `make plot` will consume the log, and generate a timing plot as `plot/timing_plot.pdf`.

**Expected Output should like**

```
====> Add(Add(A,Const), Const2) ⇒ Add(A,Add(Const,Const2))
>>> Int
[INFO-Int]: Inferred bounds: fromList [(adim,1)]
[INFO-Int]: Number of bounded verification tasks: 1
[SUCCESS-Int]: [4.37419430090813e-2s] Verification succeeded.
>>> Real
[INFO-Real]: Inferred bounds: fromList [(adim,1)]
[INFO-Real]: Number of bounded verification tasks: 1
[SUCCESS-Real]: [4.0530437996494584e-2s] Verification succeeded.
>>> Overall
[SUCCESS-Overall]: [8.427238100557588e-2s] Verification succeeded.
====> Add(A,0)⇒A
>>> Int
[INFO-Int]: Inferred bounds: fromList [(adim,1)]
[INFO-Int]: Number of bounded verification tasks: 1
[SUCCESS-Int]: [3.669909200107213e-2s] Verification succeeded.
>>> Real
[INFO-Real]: Inferred bounds: fromList [(adim,1)]
[INFO-Real]: Number of bounded verification tasks: 1
[SUCCESS-Real]: [3.956435700820293e-2s] Verification succeeded.
>>> Overall
[SUCCESS-Overall]: [7.626344900927506e-2s] Verification succeeded.
...
...
[SUCCESS-Overall]: [7.529329899989534e-2s] Verification succeeded.
Total success: 114
Total failed: 4
```

The command outputs the number of rules that the tool was and wasn't able to verify, as shown in the expected output. The paper claims that TensorRight can verify 114 out of 118 rules, and 4 rules cannot be verified. The 4 failures are explained below:

- `Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)`: TensorRight returns a counterexample for this rule (omitted from the output)
- 3 rules time out
  - `Slice(Reverse(A,dims),start,stride,end) ⇒ Reverse(Slice(A,...),dims)`
  - `Rem(Add(Iota,Const), Const) ⇒ Rem(Iota,Const)`
  - `Rem(Add(X,Const), Const) ⇒ Rem(X,Const)`

The number of timeouts could vary, depending on the host machine. The generated plot can be compared with Figure 7 in the paper.

### Claim 3.1: MaxMinToClamp Rule

The buggy rule is implemented in [maxMinToClampBefore](rules/maxMinToClampBefore/Main.hs).  
Running the following command will generate a counterexample for the rule

```bash
make maxMinToClampBefore
```

**Expected output should look like**

```
====> Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)
>>> Int
Verifying rule Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)
Verifying rule Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)
...
Model
  { i[adim.0] -> 0 :: Integer,
    map:adim.0.map -> 1 :: Integer,
    tensor:const1 -> TabularFun {funcTable = [], defaultFuncValue = 8855} :: (=->) Integer Integer,
    tensor:a -> TabularFun {funcTable = [], defaultFuncValue = 8853} :: (=->) Integer Integer,
    tensor:const2 -> TabularFun {funcTable = [], defaultFuncValue = 8854} :: (=->) Integer Integer
  }
[FAIL-Int]: [9.595382999395952e-2s] Verification failed with error: user error (  Not verified forall left si exists right si.)
...
[FAIL-Real]: [5.833859898848459e-2s] Verification failed with error: user error (  Not verified forall left si exists right si.)
>>> Overall
[FAIL-Overall]: [0.1542924289824441s] Verification failed with error: user error (  Not verified forall left si exists right si.)
```

We apply a fix to this rule, by adding the following precondition, as implemented in [maxMinToClampAfter](rules/maxMinToClampAfter/Main.hs):

```haskell
forallIdx <- newMap "forallIdx" adim
  numTensorAssumption
    [const1, const2]
    forallIdx
    (\[c1, c2] -> simpleMerge $ do
      u <- runExceptT $ tensorValLt c1 c2
      case u of
        Left _ -> con True
        Right v -> return v
      )
```

Running the following command will verify the patched rule:

```bash
make maxMinToClampAfter
```

**Expected output should look like**

```
====> Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)
>>> Int
Verifying rule Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)
Verifying rule Max(Broadcast(Const), Min(A, Broadcast(Const2))) ⇒ Clamp(A,Const,Const2)
...
[SUCCESS-Int]: [9.654362399305683e-2s] Verification succeeded.
>>> Real
...
[SUCCESS-Real]: [5.6789184993249364e-2s] Verification succeeded.
>>> Overall
[SUCCESS-Overall]: [0.1533328089863062s] Verification succeeded.
```

### Claim 3.2: Generalized Rule

**Expected Time**: 5-6 minutes

We implement the `FoldConvInputPad` rule as [conv/rule00](rules/conv/Main.hs). We show that TensorRight can prove a more general version of this rule, as implemented in [foldConvInputPadGeneral](rules/generalize/Main.hs). The following command verifies the generalized rule:

```bash
make generalize
```

**Expected output should look like**

```
====> Conv(Pad(input, innerLow, innerInt, innerHigh), weights, convLow, convInt, convHigh, rdilation)
 ⇒
Conv(input, weights, convLowOut, convIntOut, convHighOut, rdilation)
>>> Int
...
[WARNING]: Some SI cannot be accessed.
  Verified forall left si exists right si.
  Verified forall right si exists left si.
[SUCCESS-Int]: [58.718182109001646s] Verification succeeded.
>>> Real
...
[SUCCESS-Real]: [192.946455917001s] Verification succeeded.
>>> Overall
[SUCCESS-Overall]: [251.66463802600265s] Verification succeeded.
```

The generated warnings can be safely ignored, as long as the rules are verified.

## Structure of the Artifact

- [README.md](./README.md): instructions to run the artifact
- [LICENSE](./LICENSE): Apache 2.0 License file for the artifact
- [Dockerfile](Dockerfile): installation and configuration steps for the Docker image
- [stack.yaml](stack.yaml): configuration for the Haskell build tool `stack`
- [package.yaml](package.yaml): package metadata for the Haskell project
- [src/](src/): source code for TensorRight
  - [src/TensorRight/Internal/Core/](src/TensorRight/Internal/Core/): semantics of operators in TensorRight
  - [src/TensorRight/Internal/DSL/](src/TensorRight/Internal/DSL/): TensorRight DSL implementation
  - [src/TensorRight/Internal/Util/](src/TensorRight/Internal/Util/): utility functions used in TensorRight
  - [src/TensorRight.hs](src/TensorRight.hs): top-level module for TensorRight library
- [test/](test/): unit tests for TensorRight implementation
- [rules/](rules/): rewrite rules implemented in the TensorRight DSL
- [runall.sh](runall.sh): script to perform verification on 118 implemented rules
- [plot/](plot/): plotting script and results generated in Claim 2
  - [plot/timing_plot.py](plot/timing_plot.py): python script to generate the timing plot for Claim 2
- [Makefile](Makefile): commands to run the benchmarks and generate the timing plot
  - `make verify`: verifies the rewrite rules and logs the output in `plot/result.txt`
  - `make plot`: generates the timing plot `plot/timing_plot.py` using the data in `plot/result.txt`
  - `make maxMinToClampBefore`: provides the counterexample for the buggy rewrite rule [maxMinToClampBefore](rules/maxMinToClampBefore/Main.hs)
  - `make maxMinToClampAfter`: verifies the patched rewrite rule [maxMinToClampAfter](rules/maxMinToClampAfter/Main.hs)
  - `make generalize`: verifies the generalized rewrite rule [foldConvInputPadGeneral](rules/generalize/Main.hs)
  - `make build`: builds the TensorRight project if not already built
  - `make clean`: cleans the build artifacts
