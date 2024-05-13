---
category: tool
tool: HCL
contributors:
    - ["Romans Malinovskis" , "http://github.com/romaninsh"]
filename: terraform.txt
---
## Introduction

HCL (Hashicorp Configuration Language) is a high-level configuration language used in tools from
Hashicorp (such as Terraform). HCL/Terraform is widely used in provisioning cloud infastructure and
configuring platforms/services through APIs. This document focuses on HCL 0.13 syntax.

HCL is a declarative language and Terraform will consume all `*.tf` files in the current folder, so code
placement and sequence has no significance. Sub-folders can be consumed through modules.

This guide is focused on HCL specifics, you should already be familiar with what Terraform is.

```terraform
// Top-level HCL file will interactively ask user values for the variables
// which do not have a default value
variable "ready" {
  description = "Ready to learn?"
  type = bool
  // default = true
}

// Module block consults a specified folder for *.tf files, would
// effectively prefix all resources IDs with "module.learn-basics."
module "learn-basics" {
  source = "./learn-basics"
  ready_to_learn = var.ready
}

output "knowledge" {
  value = module.learn-basics.knowledge
}
```

## learn-basics

```terraform
// Variables are not automatically passed into modules
// and can be typeless.
variable "ready" {
}

// It is good practice to define a type though. There are 3 primitive types -
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

// Collection types may specify a type, but can also be "any".
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

// When the type is not specified or is a mix of scalars
// they will be converted to strings.

// Use modern IDEs for type completion features. It does not matter
// in which file and in which order you define a variable, it becomes
// accessible from anywhere.

// Default values for variables may not use expressions, but you can
// use locals for that. You don't specify types for locals. With locals
// you can create intermediate products from other variables, modules,
// and functions.

locals {
  ready = var.ready ? "yes": "no"

  yaml = yamldecode(file("${path.module}/file-in-current-folder.yaml"))
}

// 'locals' blocks can be defined multiple times, but all variables,
// resources and local names should be unique

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

Terraform exists for managing cloud "resources". A resource could be anything as long as it
can be created and destroyed through an API call. (compute instance, distribution,
DNS record, S3 bucket, SSL certificate or permission grant). Terraform relies on "providers"
for implementing specific vendor APIs. For example the "aws" provider enables use of resources
for managing AWS cloud resources.

When `terraform` is invoked (`terraform apply`) it will validate code, create all resources
in memory, load their existing state from a file (state file), refresh against the current
cloud APIs and then calculate the differences. Based on the differences, Terraform proposes
a "plan" - series of create, modify or delete actions to bring your infrastructrue in
alignment with an HCL definition.

Terraform will also automatically calculate dependencies between resources and will maintain
the correct create / destroy order. Failure during execution allows you to retry the entire
process, which will usually pick off where things finished.

## more-learning

Time to introduce resources.

```terraform
variable "yaml-data" {

  // config is sourced from a .yaml file, so technically it is a
  // map(any), but we can narrow down type like this:
  type = map(string)
}

// You do not need to explicitly define providers, they all have reasonable
// defaults with environment variables. Using a resource that relies on a
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
  // will be available right away (like bucket) and others may only
  // become available after the plan begins executing. The test-file resource
  // will be created only after aws_s3_bucket.bucket finishes being created

  // depends_on = aws_s3_bucket.bucket
  bucket = aws_s3_bucket.bucket.bucket
  key = "index.html"
  content = file("${path.module}/index.html")

  // you can also manually specify provider alias
  provider = aws.as-role
}

// Each resource will receive an ID in state, like "aws_s3_bucket.bucket".
// When resources are created inside a module, their state ID is prepended
// with module.<module-name>

module "learn-each" {
  source = "../learn-each"
}

// Nesting modules like this may not be the best practice, and it's only
// used here for illustration purposes
```

## learn-each

Terraform offers some great features for creating series of objects:

```terraform
locals {
  list = ["red", "green", "blue"]
}
resource "aws_s3_bucket" "badly-coloured-bucket" {
  count = count(local.list)
  bucket_prefix = "${local.list[count.index]}-"
}
// will create 3 buckets, prefixed with "red-", etc. and followed by
// a unique identifier. Some resources will automatically generate
// a random name if not specified. The actual name of the resource
// (or bucket in this example) can be referenced as attributes

output "red-bucket-name" {
  value = aws_s3_bucket.badly-coloured-bucket[0].bucket
}

// note that bucket resource ID will be "aws_s3_bucket.badly-coloured-bucket[0]"
// through to 2, because they are list index elements. If you remove "red" from
// the list, however, it will re-create all the buckets as they would now
// have new IDs. A better way is to use for_each

resource "aws_s3_bucket" "coloured-bucket" {
  // for_each only supports maps and sets
  for_each = toset(local.list)
  bucket_prefix = "${each.value}-"
}

// the name for this resource would be aws_s3_bucket.coloured-bucket[red]

output "red-bucket-name2" {
  value = aws_s3_bucket.badly-coloured-bucket["red"].bucket
}

output "all-bucket-names" {

  // returns a list containing bucket names - using a "splat expression"
  value = aws_s3_bucket.coloured-bucket[*].bucket
}

// there are other splat expressions:
output "all-bucket-names2" {
  value = [for b in aws_s3_bucket.coloured-bucket: b.bucket]
}
// can also include a filter
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

// as of Terraform 0.13 it is now also possible to use count/each for modules

variable "learn-functions" {
  type = bool
  default = true
}

module "learn-functions" {
  count = var.learn-functions ? 1: 0
  source = "../learn-functions"
}
```

This is now popular syntax that works in Terraform 0.13 that allows including modules conditionally.

## learn-functions

Terraform does not allow you to define your own functions, but there's an extensive list of built-in functions

```terraform
locals {
  list = ["one", "two", "three"]

  upper_list = [for x in local.list : upper(x) ] // "ONE", "TWO", "THREE"

  map = {for x in local.list : x => upper(x) } // "one":"ONE", "two":"TWO", "three":"THREE"

  filtered_list = [for k, v in local.map : substr(v, 0, 2) if k != "two" } // "ON", "TH"

  prefixed_list = [for v in local.filtered_list : "pre-${k}" } // "pre-ON", "pre-TH"

  joined_list = join(local.upper_list,local. filtered_list) // "ONE", "TWO", "THREE", "pre-ON", "pre-TH"

  // Set is very similar to List, but element order is irrelevant
  joined_set = toset(local.joined_list) // "ONE", "TWO", "THREE", "pre-ON", "pre-TH"

  map_again = map(slice(local.joined_list, 0, 4)) // "ONE":"TWO", "THREE":"pre-ON"
}

// Usually list manipulation can be useful either for a resource with for_each or
// to specify a dynamic block for a resource. This creates a bucket with some tags:

resource "aws_s3_bucket" "bucket" {
  name = "test-bucket"
  tags = local.map_again
}

// this is identical to:
// resource "aws_s3_bucket" "bucket" {
//   name = "test-bucket"
//   tags = {
//     ONE = "TWO"
//     THREE = "pre-ON"
//   }
// }

// Some resources also contain dynamic blocks. The next example uses a "data" block
// to look up 3 buckets (red, green and blue), then creates a policy that contains
// read-only access to the red and green buckets and full access to the blue bucket.

locals {
  buckets = {
    red = "read-only"
    green = "read-only"
    blue = "full"
  }
  // we could load buckets from a file:
  // bucket = file('bucket.json')

  actions = {
    "read-only" = ["s3:GetObject", "s3:GetObjectVersion"],
    "full" = ["s3:GetObject", "s3:GetObjectVersion", "s3:PutObject", "s3:PutObjectVersion"]
  }
  // we will look up actions, so that we don't have to repeat actions
}

// use a function to convert map keys into set
data "aws_s3_bucket" "bucket" {
  for_each = toset(keys(local.buckets))
  bucket = each.value
}

// create json for our policy
data "aws_iam_policy_document" "role_policy" {
  statement {
    effect = "Allow"
    actions = [
      "ec2:*",
    ]
    resources = ["*"]
  }

  dynamic "statement" {
    for_each = local.buckets
    content {
      effect = "Allow"
      actions = lookup(local.actions, statement.value, null)
      resources = [data.aws_s3_bucket.bucket[statement.key]]
    }
  }
}

// and this actually creates the AWS policy with permissions to all buckets
resource "aws_iam_policy" "policy" {
  policy = data.aws_iam_policy_document.role_policy.json
}
```

## Additional Resources

- [Terraform tips & tricks](https://blog.gruntwork.io/terraform-tips-tricks-loops-if-statements-and-gotchas-f739bbae55f9)
- [Building Dynamic Outputs with Terraform Expressions and Functions](https://www.thegreatcodeadventure.com/building-dynamic-outputs-with-terraform-for_each-for-and-zipmap/)
