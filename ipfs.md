---
category: tool
name: IPFS
filename: ipfs.bash
contributors:
    - ["Ross Spencer", "http://github.com/ross-spencer"]
---

<!-- [ipfs/en] InterPlanetary File System -->

InterPlanetary File System (IPFS) is a decentralized peer-to-peer file- and
data- sharing network that uses content addressable storage to create
a global namespace for content.

[kubo][kubo-1] provides an official reference implementation for engaging with
IPFS on the command line. Once installed you will have access to the `ipfs`
command.

## Initialize IPFS

```bash
# Initialize your computer as an IPFS node with:
./ipfs init

# Output
# generating ED25519 keypair...done
# peer identity: 12D3KooWQQe2yFFNE4iqMz1HFBhd2CGN4f5XD9MgB4YNBLyjsqt8
# initializing IPFS node at /home/user/.ipfs

# Inspect the initialized IPFS root:
$ tree ~/.ipfs

# Output
# /home/user/.ipfs
# ├── blocks
# │   ├── diskUsage.cache
# │   ├── _README
# │   ├── SHARDING
# │   └── X3
# │       └── CIQFTFEEHEDF6KLBT32BFAGLXEZL4UWFNWM4LFTLMXQBCERZ6CMLX3Y.data
# ├── config
# ├── datastore
# │   ├── 000001.log
# │   ├── CURRENT
# │   ├── LOCK
# │   ├── LOG
# │   └── MANIFEST-000000
# ├── datastore_spec
# ├── keystore
# └── version

```

## Start the IPFS daemon

The IPFS daemon is vital for propagating your data. Try your IPFS commands
both with and without it running to undderstand its impact.

```bash
./ipfs daemon

# Output
# Initializing daemon...
# ...
# Daemon is ready

# NB. Open another terminal window, to continue
```

## Add and retrieve data from IPFS

```bash
# Add a string to IPFS:
echo "Hello Learn X in Y minutes" | ./ipfs add

# Output
# added QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1 QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1

./ipfs cat QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1

# Output
# Hello Learn X in Y minutes
```

## Read the data via cURL

```bash
# Convert your v0 identifier to base32:
./ipfs cid base32 QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1

# Output
# bafybeih3othzkow5sr2t6q7smwzrrfgitw277ytesdkb43z527afnhwoqq

# Request the data from an IPFS Gateway:
curl https://bafybeih3othzkow5sr2t6q7smwzrrfgitw277ytesdkb43z527afnhwoqq.ipfs.dweb.link

# Output
# Hello Learn X in Y minutes

# Do the same via another gateway:
curl https://ipfs.io/ipfs/bafybeih3othzkow5sr2t6q7smwzrrfgitw277ytesdkb43z527afnhwoqq

# Output
# Hello Learn X in Y minutes

# Create a CID from the outset:
echo "crerating a CID from the outset" | ./ipfs add --cid-version 1

# Output
# added bafkreie5gakawm2czygbrzprxl56eo5cv76tlkn6ckytncb7hdyy4mqfsu bafkreie5gakawm2czygbrzprxl56eo5cv76tlkn6ckytncb7hdyy4mqfsu

```

## View data already in the IPFS

```bash

# Because we know its address up front we can download and view XKCD 927:
./ipfs get QmTJc66pWBfz8oaS2dQhQqq3UJex7E9r3jTVswHifzD76p

# Output
# Saving file(s) to QmTJc66pWBfz8oaS2dQhQqq3UJex7E9r3jTVswHifzD76p

tree QmTJc66pWBfz8oaS2dQhQqq3UJex7E9r3jTVswHifzD76p

# Output
# QmTJc66pWBfz8oaS2dQhQqq3UJex7E9r3jTVswHifzD76p
# ├── 927 - Standards - alt.txt
# ├── 927 - Standards.png
# └── 927 - Standards - transcript.txt

# These can all be viewed locally with nix tooling, or we can continue to use
# IPFS:
./ipfs cat QmaHkEgzRdocAK3bvZWjhxvoHv2kAKkmMwyoQcxbUGVKxg | display

# Output
# Will display in `display` if installed with ImageMagick.

# And its transcript:
./ipfs cat QmUvEch4som4WK7dmE5pLoM3GBA7nCeqms8oqKyaaF9Wgy

# Output
# HOW STANDARDS PROLIFERATE
# (See: A
# C chargers, character encodings, instant messaging, etc.)
#
# SITUATION:
# There are 14 competing standards.
#
# Geek: 14?! Ridiculous! We need to develop one universal standard that covers everyone's use cases.
# Fellow Geek: Yeah!
#
# Soon:
# SITUATION:
# There are 15 competing standards.
#
# {{Title text: Fortunately, the charging one has been solved now that we've all standardized on mini-USB. Or is it micro-USB? Shit.}}

# Its alt-text:
./ipfs cat QmfNvocxez2nEChH95BQCLhJLciy5dmwbGW4NofTUvNvgu

# Output
# Fortunately, the charging one has been solved now that we've all standardized on mini-USB. Or is it micro-USB? Shit.

```

## List local data (CIDs)

The information IPFS stores asks you to think differently about your files.
Despite the FS in the name (IP)FS it's more analogous to a system for
managing data that is identified by its content identifier CID. IPFS
allows you, and others to retrieve data that is being provided and asks
the user to interpret it correctly.

```bash
# List the files that you are "providing" / "hosting" locally that others can
# access via the IPFS:
./ipfs pin ls

# Output
# QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn recursive
# QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1 recursive

# Not all of these will be the files you added, so inspect them.to understand
# what they are:
./ipfs files stat /ipfs/QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1

# Output
# QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1
# Size: 27
# CumulativeSize: 35
# ChildBlocks: 0
# Type: file
# Mode: not set (not set)
# Mtime: not set
```

## The importance of indexing

As you make use of it, you might consider how you index the data you
provide to the IPFS so that others can find it, but more importantly,
that you retain intellectual and curatorial control over it.

## IPNS

InterPlanetary Name System (IPNS) provides one method for you to think
about an index. An IPNS provides a persistent identifier for data that is
mutable, that is, you can update content and the identifier remains the same.

```bash
# Write some new data:
echo "[1]" | ./ipfs add --cid-version 1

# Output
# added bafkreifmyb5wf4r7iwesg434jtkkm26qluphd22phbaahovpfxghmdldje bafkreifmyb5wf4r7iwesg434jtkkm26qluphd22phbaahovpfxghmdldje

# Publish the data to your default IPNS:
./ipfs name publish /ipfs/bafkreifmyb5wf4r7iwesg434jtkkm26qluphd22phbaahovpfxghmdldje

# Output
# Published to k51qzi5uqu5dhl7wp9jatxst1ke5ms4l0cp26yjk4orf4t7dr5xvj4u8pj0ib0: /ipfs/bafkreifmyb5wf4r7iwesg434jtkkm26qluphd22phbaahovpfxghmdldje

# View your file via a gateway:
curl https://ipfs.io/ipns/k51qzi5uqu5dhl7wp9jatxst1ke5ms4l0cp26yjk4orf4t7dr5xvj4u8pj0ib0

# Output
# [1]

# Change your data and publish it to your IPNS:
echo "[1, 2]" | ./ipfs add --cid-version 1

# Output
# added bafkreia7wp746ppytyy7k2js2steui7o32fc6zyhrgf7cti7yqvhqkdina bafkreia7wp746ppytyy7k2js2steui7o32fc6zyhrgf7cti7yqvhqkdina

# Publish the new CID:
./ipfs name publish bafkreia7wp746ppytyy7k2js2steui7o32fc6zyhrgf7cti7yqvhqkdina

# Output
# Published to k51qzi5uqu5dhl7wp9jatxst1ke5ms4l0cp26yjk4orf4t7dr5xvj4u8pj0ib0: /ipfs/bafkreia7wp746ppytyy7k2js2steui7o32fc6zyhrgf7cti7yqvhqkdina

# Wait 5-10 minutes and poll the gateway again:
curl https://ipfs.io/ipns/k51qzi5uqu5dhl7wp9jatxst1ke5ms4l0cp26yjk4orf4t7dr5xvj4u8pj0ib0

# Output
# [1, 2]

# Other IPNS names can be used for more granular control of persistent
# IPNS links, e.g., different content; your blog, homepage, media collection,
# and so on:
./ipfs key gen my-new-key

# Output
# k51qzi5uqu5dkyu7b6rdcayq3tvmfwqh90hjea80n5p9ih4p3amxar98c1r0wp

./ipfs name publish /ipfs/bafkreidhgwhnr7lm2tmkvqz2zkrxnnmcg67lreod5gcokbot4v7eh56tea --key my-new-key

# Output
# Published to k51qzi5uqu5dkyu7b6rdcayq3tvmfwqh90hjea80n5p9ih4p3amxar98c1r0wp: /ipfs/bafkreidhgwhnr7lm2tmkvqz2zkrxnnmcg67lreod5gcokbot4v7eh56tea

```

### Signing

We saw with pinning that IPFS has keys and so it can potentially be used for data signing.

```bash
# The key `my-new-key`: k51qzi5uqu5dkyu7b6rdcayq3tvmfwqh90hjea80n5p9ih4p3amxar98c1r0wp
# is an ed25519 key.

# Sign data and publish it as follows:
DATA_TO_SIGN="Hello security!"
echo $DATA_TO_SIGN | ./ipfs add && echo $DATA_TO_SIGN | ./ipfs key sign --key my-new-key | ./ipfs add

# Output
# added QmaQhz8NPzZqLsu4b8ZgNzue1SaEAMgz4FV282kRez1F7P QmaQhz8NPzZqLsu4b8ZgNzue1SaEAMgz4FV282kRez1F7P
# added QmNLh2WNKVArkDo88rJozDMRTAQxLBtGU2xHmJVZ13yK9M QmNLh2WNKVArkDo88rJozDMRTAQxLBtGU2xHmJVZ13yK9M

# Imagine you alreaday know of the public key from a trusted party.

# Verify signed data retrieve from IPFS:
./ipfs cat QmaQhz8NPzZqLsu4b8ZgNzue1SaEAMgz4FV282kRez1F7P | \
 ./ipfs key verify \
 --signature $(./ipfs cat QmNLh2WNKVArkDo88rJozDMRTAQxLBtGU2xHmJVZ13yK9M | \
 jq -r .Signature) \
 --key k51qzi5uqu5dkyu7b6rdcayq3tvmfwqh90hjea80n5p9ih4p3amxar98c1r0wp | jq

# It's a little clunky but it may provide a useful demonstration for
# you to build on.

# Output
# {
#   "Key": {
#     "Name": "",
#     "Id": "k51qzi5uqu5dkyu7b6rdcayq3tvmfwqh90hjea80n5p9ih4p3amxar98c1r0wp"
#   },
#   "SignatureValid": true
# }

```

## Pinning

Persistence of data is ensured by a mechanism called pinning.

```bash
# List all locally pinned files:
./ipfs pin ls --type=all

# Output
# QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1 recursive
# bafkreie5gakawm2czygbrzprxl56eo5cv76tlkn6ckytncb7hdyy4mqfsu recursive

# Garbage collect (gc) what's already available to gc:
./ipfs repo gc

# Output
# removed bafkreih5fqe4ew6ir2inqa3u76ize2yg3nsxw3m5wmfk25ijmqf4payioy
# removed bafkreidbycqgtvz2ph7o5cu4jktzyy2k544k6fod73gfybqfk2moy7wuei
# removed bafkreigfrvnk6clikkwxjftgxyy6nfo6ocekd5sqpp34yjereup4wul4jm
# removed bafkreifrrxabyunbp4l4ictsyfzhh32l6j7srpp472kr3dnj5rkl2ztiku
# removed bafkreicjyrhuoztkpfcjl3dyhv6gstkcsgbgurfah3s4s3fi6adolgb2te
# removed bafkreicghp2nkqdxabzeo6u4vrcn3pxmx7v6fnaj5lpuvnsgrj7tukbl4y

# Unpin one of your own test files:
./ipfs pin rm QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1

# Output
# unpinned QmfGEMEzFrHB3e9RwZvboUmAL9tY6KVvKEcWgiHWxvY9f1

# Content is removed locally, and may only be retrievable globally if
# cached by another peer.
```

For long-term duplication of your CIDs you ineed to stand-up your own
pinning API or make use of a third-parrty pinning service.

## IPLD

InterPlanetary Linked Data is a data first approach to looking at the
mechanisms provided by IPFS.

```bash
# Add JSON directly to IPLD:
echo '{"Hello": "Learn X in Y minutes" }' | ./ipfs dag put

# Output
# bafyreiatmqgvdvcum4w4lwy2cwzixweoshjfvuy6pcoixhd5hz4imrohse

./ipfs dag get bafyreiatmqgvdvcum4w4lwy2cwzixweoshjfvuy6pcoixhd5hz4imrohse | jq

# Output
# {
#   "Hello": "Learn X in Y minutes"
# }

# You can also output CBOR:
./ipfs dag get bafyreiatmqgvdvcum4w4lwy2cwzixweoshjfvuy6pcoixhd5hz4imrp --output-codec cbor | xxd -p

# Output
# a16548656c6c6f744c6561726e205820696e2059206d696e75746573

# Which looks as follows in CBOR playground:
#
# A1                                      # map(1)
#    65                                   # text(5)
#       48656C6C6F                        # "Hello"
#    74                                   # text(20)
#       4C6561726E205820696E2059206D696E75746573 # "Learn X in Y minutes"

# The largest benefit might be storing data as data.
# But if you do have interconnected data you can model it by linking
# back to previous DAG CIDs.
echo '{"foo": "bar", "prev_cid": "bafyreiatmqgvdvcum4w4lwy2cwzixweoshjfvuy6pcoixhd5hz4imrohse" }' \
 | ./ipfs dag put

# Output
# bafyreihl77szrg4z6ftm23z43ymurqtpggtypln5qndhw36s4diludlyli

# Retrieve the data:
./ipfs dag get bafyreihl77szrg4z6ftm23z43ymurqtpggtypln5qndhw36s4diludlyli --output-codec json

# Output
# {
# 	"foo": "bar",
# 	"prev_cid": "bafyreiatmqgvdvcum4w4lwy2cwzixweoshjfvuy6pcoixhd5hz4imrohse"
# }

# You must input "data", e.g. JSON, not raw bytes that IPLD is able to
# parse and re-present back to users as anticipated.

# Input invalid data, i.e. plain bytes:
echo "Hello Learn X in Y minutes" | ./ipfs dag put

# Output
# Error: Invalid byte while expecting start of value: 0x48

# Input valid data:
$ echo "{}" | ./ipfs dag put

# Output
# bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua

```

These are just baby steps into IPLD. Take a look at the IPLD documentation
for more information and how you might implement it.

IPLD is complicated and so [DASL][dasl-1] has been created as a subset of
the standard and its primitives to help bridge the gap in use.

## The IPFS ecosystem

At the time of writing there are over 200 commands and infinite possibilities.
IPFS provides an entire ecosystem of tooling with many commands you will
need to look further into.

```bash
# All commands can be output with:
./ipfs commands

# Count them as follows:
./ipfs commands | wc -l

# Output
# 201 (commands)
```

## Utilities

* [CID checker][cid-checker-1]
* [IPFS retrieval check][ipfs-retrieval-1]

## Further reading

* [IPFS docs][ipfs-docs-1]
* [List of IPFS gateways][gateways-1 ]
* [IPNS documentation][ipns-1]
* [IPLD homepage and documentation][ipld-1]
* [IPLD explorer][explorer-1]
* [Pinning][pinning-1]
* [Glossary][glossary-1]

<!-- Inline references -->
[dasl-1]: https://dasl.ing/
[kubo-1]: https://github.com/ipfs/kubo

<!-- Utilities -->
[cid-checker-1]: https://cid.ipfs.tech/
[ipfs-retrieval-1]: https://check.ipfs.network

<!-- Further reading -->
[explorer-1]: https://explore.ipld.io/
[gateways-1]: https://ipfs.github.io/public-gateway-checker/
[glossary-1]: https://docs.ipfs.tech/concepts/glossary/
[ipfs-docs-1]: https://docs.ipfs.tech/
[ipld-1]: https://ipld.io/
[ipns-1]: https://docs.ipfs.tech/how-to/publish-ipns/#publishing-ipns-names-with-kubo
[pinning-1]: https://docs.ipfs.tech/how-to/pin-files/#three-kinds-of-pins
