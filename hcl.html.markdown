---
category: tool
tool: hcl
contributors:
    - ["Romans Malinovskis" , "http://github.com/romaninsh"]
filename: terraform.txt
---

## Introduction

HCL (Hashicorp Configuration Language) is a high-level configuration language used in tools from 
Hashicorp (such as Terraform). This document focuses on a most recent HCL syntax (0.12).

HCL is a declarative language and will consume all `*.tf` in the current folder. Sub-folders
can be consumed through modules.

This guide is focused on HCL specifics, you should be already familiar with what is Terraform.

```hcl-terraform
// Top-level HCL file will interactively ask user values for the variables
// which do not have default value
variable "ready" {
  description = "Ready to learn?"
  type = bool
  // default = true
}

// Module block consults a specified folder for *.tf files, would
// effectively prefix all resources id's with "module.learn-basics."
// 
module "learn-basics" {
  source = "./learn-basics"
  ready_to_learn = var.ready
}

output "knowledge" {
  value = module.learn-basics.knowledge
}
```

## learn-basics

```hcl-terraform

// Variables are not automatically passed into modules
// and can be typeless.
variable "ready" {
}

// It is good practice to define type though. There are 3 primitive types
// 3 collection types and 2 structural types. Structural types define
// types recursively
variable "structural-types" {
  type = object({
    object: object({
      can-be-nested: bool
    }),
    tuple: tuple([int, string])
  })
  
  default = {
    object = { can-be-nested: true }
    tuple = [3, "cm"]
  }
}

// Collection types may specify type, but can also be "any".
variable "list" {
  type: list(string)
  default = ["red", "green", "blue"]
}

variable "map" {
  type: map(any)
  default = {
    red = "#FF0000"
    "green" = "#00FF00"
  }
}

variable "favourites" {
  type: set
  default = ["red", "blue"]
}

// When type is not specified or is mixed of scalars they will be
// converted to strings.


// IDEs (such as IntelliJ) syntax highlighting
// plugin would offer type completion features. It does not matter
// in which file and in which order you define variable, it becomes
// accessible from anywhere.

// Default value for variables may not use expressions, but you can
// use locals for that. You don't specify types for locals. With locals
// you can create intermediate products from other variables, modules,
// and functions.

locals {
  ready = var.ready ? "yes": "no"

  yaml = yamldecode(file("${path.module}/file-in-current-folder.yaml"))
}

// locals block can be defined multiple times, but all variable, resource
// and local names should be unique 

locals {
  set = toset(var.map)
}

module "more-resources" {
  source = "../more-learning"
  yaml-data = local.yaml
}

// Modules can declare outputs, that can be optionally referenced
// (see above), typically outputs appear at the bottom of the file or
// in "outputs.tf".
output "knowledge" {
  value = "types so far, more to come"
}
```

Terraform exist for managing "resources". A could be anything as long as it
can be created and destroyed through an API call. (compute instance, distribution,
dns record, S3 bucket). Terraform provides list of official "providers" and
there are third party providers available too.

When terraform apply is invoked, it will calculate all the resources and
use provider API to ensure their state is as close to described state
as possible.

```hcl-terraform
variable "yaml-data" {

  // config is sourced from .yaml file, so technically it is a
  // map(any), but we can narrow down type like this:
  type = map(string)
}

// You do not need to explicitly define providers, they all have reasonable
// defaults with environment variables. Using resource that relies on
// provider will also transparently initialize it (when you invoke terraform init)
resource "aws_s3_bucket" "bucket" {
  bucket = "abc"
}

// You can also create provider aliases
provider "aws" {
  alias = "as-role"
  assume_role {
    role_arn = ".."
  }
}

// then use them to create resources
resource "aws_s3_bucket_object" "test-file" {

  // all resources have attributes that can be referenced. Some of those
  // would be available right away (like bucket) and others may only
  // become available during apply phase. Terraform automatically
  // determine dependencies, although you could still specify depends_on
  // when there is no direct dependency

  // depends_on = aws_s3_bucket.bucket
  bucket = aws_s3_bucket.bucket.bucket
  key = "index.html"
  content = file("${path.module}/index.html")

  // you can also manually specify provider alias
  provider = aws.as-role
}

// Normally it's not a very good practice to nest modules, as this
// creates incredibly long identifiers for your resources, but
// I'm doing it here with learning spirit.

module "learn-each" {
  source = "../learn-each"
}

```

## learn-each

Terraform offers some great features for creating series of objects:

```hcl-terraform
locals {
  list = ["red", "green", "blue"]
}
resource "aws_s3_bucket" "badly-coloured-bucket" {
  count = count(local.list)
  bucket_prefix = "${local.list[count.index]}-"
}
// will create 3 buckets, prefixing with "red-", etc and followed by
// a unique identifier. Some resources will automatically generate
// random name if not specified. The actual name of the resource
// (or bucket in my example) can be referenced as attribute

output "red-bucket-name" {
  value = aws_s3_bucket.badly-coloured-bucket[0].bucket
}

// note that bucket resource id's will be "aws_s3_bucket.badly-coloured-bucket.0"
// through to 2, because list indexes elements. If you remove "red" from
// the list, however, it will re-create all the buckets as they would now
// have new ids. A better way is to use for_each

resource "aws_s3_bucket" "coloured-bucket" {
  // for_each only supports maps and sets
  for_each = toset(local.list)
  bucket_prefix = "${each.value}-"
}
output "red-bucket-name2" {
  value = aws_s3_bucket.badly-coloured-bucket["red"].bucket
}

output "all-bucket-names" {

  // returns a list containing bucket names - using "splat expression"
  value = aws_s3_bucket.coloured-bucket[*].bucket
}

// there are other splat expressions:
output "all-bucket-names2" {
  value = [for b in aws_s3_bucket.coloured-bucket: b.bucket]
}
// can also include filter
output "filtered-bucket-names" {
  value = [for b in aws_s3_bucket.coloured-bucket: 
    b.bucket if length(b.bucket) < 10 ]
}

// here are some ways to generate maps {red: "red-123123.."}
output "bucket-map" {
  value = {
    for b in aws_s3_bucket.coloured-bucket: 
       trimsuffix(b.bucket_prefix, '-')
         => b.bucket
   }
}


```

I will contribute more to this guide when I have time

## Additional Resources

https://blog.gruntwork.io/terraform-tips-tricks-loops-if-statements-and-gotchas-f739bbae55f9
https://www.thegreatcodeadventure.com/building-dynamic-outputs-with-terraform-for_each-for-and-zipmap/

