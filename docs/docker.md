# Docker images

Our CI pipeline publishes Docker images for each executable we build, tagged with
the branch name and commit hash. Only images built from the `master` branch are
tagged with the default `latest` tag.

```console
docker run eu.gcr.io/opensourcecoin/oscoin --help
docker run eu.gcr.io/opensourcecoin/oscoin:my-branch-name --help
docker run eu.gcr.io/opensourcecoin/oscoin:deabeefcommithash --help
```

Listing the Docker images that are published to our container registry can be done
via the [web-browser](https://console.cloud.google.com/gcr/images/opensourcecoin?project=opensourcecoin)
or with the `gcloud` CLI.

First, authenticate if you haven't yet:
```console
gcloud auth login
gcloud config set project opensourcecoin
```

Then list the images in the repository:
```console
gcloud container images list --repository eu.gcr.io/opensourcecoin

NAME
eu.gcr.io/opensourcecoin/client
eu.gcr.io/opensourcecoin/git-remote-oss
eu.gcr.io/opensourcecoin/oscoin
eu.gcr.io/opensourcecoin/oscoin-cli
eu.gcr.io/opensourcecoin/radicle
eu.gcr.io/opensourcecoin/radicle-server
eu.gcr.io/opensourcecoin/server
```

If you need to see all tags a single image has:
```console
gcloud container images list-tags eu.gcr.io/opensourcecoin/oscoin

DIGEST        TAGS                                                 TIMESTAMP
f85ff8bbf253  71e79cb88392635e138f7a52fbe0740e1132ba09,dawg-build  2018-10-09T18:20:31
0c1d9c1b9c4f  c9fefac4557b0c3ea7ad1549e1f6aa3f8b455fdd             2018-10-09T18:13:44
b41e113c1eef  76eabab8c82c031b08a2bef75d33a13a02199b82             2018-10-09T18:13:03
3352fdaad354  e88b746a2cbfb7b43aa086e707e6fca3f9b32b30             2018-10-09T17:58:32
afced8bd80e3  900f2f7992d36b7aee014eb05b080636686be863             2018-10-09T17:29:07
98750be93b1b  6be363f1797b7084ac43a65574ae947695f294f5             2018-10-09T17:22:50
b3864d2ece9c  d7c75b3                                              2018-10-09T12:38:43
99122ae6c18d                                                       2018-10-09T12:36:20
3b05482a0e75  d0450e79b1c4e13d9da295084895760516be1b1f             2018-10-08T18:16:31
66be31145f55  8f1077b3f384cbecf51f5d7b080a9f46ea8c7485             2018-10-08T18:04:16
5e95a7ebaef9  e86e73089f39bc60a8f763cf8ce061752dfbdaca             2018-10-08T17:04:10
```

## Running

The entrypoint of images is the executable itself, so they can be invoked like
a simple CLI command, with support for flags.

```console
docker run eu.gcr.io/opensourcecoin/oscoin --help
Usage: oscoin [-h|--host ARG] [--gossip-port ARG] [--api-port ARG] [--seeds ARG]
              [--prelude ARG] [--difficulty ARG]
  Oscoin Node

Available options:
  -h,--help                Show this help text
  -h,--host ARG            Host name to bind to for
                           gossip (default: "127.0.0.1")
  --gossip-port ARG        Port number to bind to for gossip (default: 6942)
  --api-port ARG           Port number to bind to for the HTTP
                           API (default: 8477)
  --seeds ARG              Path to YAML file describing gossip seed nodes
  --prelude ARG            Path to radicle prelude
  --difficulty ARG         Mining difficulty
```

If an image expects certain paths to exists at run-time, those need to be volume mounted (e.g. private keys).
Ports need to be exposed with the `--publish` flag so that host programs can talk to the program
inside the container.

```console
docker run --workdir "/workspace" \
           --volume "$(pwd)/node:/workspace/node" \
           --volume "$HOME/.config/oscoin:/.config/oscoin" \
           --publish 6942:6942 \
           --publish 8477:8477 \
           eu.gcr.io/opensourcecoin/oscoin
```
