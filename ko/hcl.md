---
category: tool
name: HCL
contributors:
    - ["Romans Malinovskis" , "http://github.com/romaninsh"]
filename: terraform.txt
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

## 소개

HCL(Hashicorp 구성 언어)은 Hashicorp의 도구(예: Terraform)에서 사용되는 고급 구성 언어입니다. HCL/Terraform은 클라우드 인프라를 프로비저닝하고 API를 통해 플랫폼/서비스를 구성하는 데 널리 사용됩니다. 이 문서는 HCL 0.13 구문에 중점을 둡니다.

HCL은 선언적 언어이며 Terraform은 현재 폴더의 모든 `*.tf` 파일을 사용하므로 코드 배치 및 순서는 중요하지 않습니다. 하위 폴더는 모듈을 통해 사용할 수 있습니다.

이 가이드는 HCL 세부 정보에 중점을 두므로 Terraform이 무엇인지 이미 알고 있어야 합니다.

```terraform
// 최상위 HCL 파일은 기본값이 없는 변수에 대해 사용자 값을 대화식으로 묻습니다.
variable "ready" {
  description = "배울 준비가 되셨습니까?"
  type = bool
  // default = true
}

// 모듈 블록은 지정된 폴더에서 *.tf 파일을 참조하며, 모든 리소스 ID에 "module.learn-basics." 접두사를 효과적으로 붙입니다.
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
// 변수는 모듈에 자동으로 전달되지 않으며 유형이 없을 수 있습니다.
variable "ready" {
}

// 유형을 정의하는 것이 좋습니다. 3개의 기본 유형, 3개의 컬렉션 유형 및 2개의 구조적 유형이 있습니다. 구조적 유형은 유형을 재귀적으로 정의합니다.
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

// 컬렉션 유형은 유형을 지정할 수 있지만 "any"일 수도 있습니다.
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

// 유형이 지정되지 않았거나 스칼라가 혼합된 경우 문자열로 변환됩니다.

// 유형 완성 기능을 위해 최신 IDE를 사용하십시오. 어떤 파일과 어떤 순서로 변수를 정의하든 상관없이 어디에서나 액세스할 수 있습니다.

// 변수의 기본값은 표현식을 사용할 수 없지만, 이를 위해 로컬을 사용할 수 있습니다. 로컬에 대한 유형을 지정하지 않습니다. 로컬을 사용하면 다른 변수, 모듈 및 함수에서 중간 제품을 만들 수 있습니다.

locals {
  ready = var.ready ? "yes": "no"

  yaml = yamldecode(file("${path.module}/file-in-current-folder.yaml"))
}

// 'locals' 블록은 여러 번 정의할 수 있지만 모든 변수, 리소스 및 로컬 이름은 고유해야 합니다.

locals {
  set = toset(var.map)
}

module "more-resources" {
  source = "../more-learning"
  yaml-data = local.yaml
}

// 모듈은 선택적으로 참조할 수 있는 출력을 선언할 수 있습니다(위 참조). 일반적으로 출력은 파일 하단이나 "outputs.tf"에 나타납니다.
output "knowledge" {
  value = "types so far, more to come"
}
```

Terraform은 클라우드 "리소스"를 관리하기 위해 존재합니다. 리소스는 API 호출을 통해 생성 및 소멸될 수 있는 모든 것이 될 수 있습니다. (컴퓨팅 인스턴스, 배포, DNS 레코드, S3 버킷, SSL 인증서 또는 권한 부여). Terraform은 특정 공급업체 API를 구현하기 위해 "공급자"에 의존합니다. 예를 들어 "aws" 공급자는 AWS 클라우드 리소스를 관리하기 위한 리소스 사용을 가능하게 합니다.

`terraform`이 호출되면(`terraform apply`) 코드를 검증하고, 메모리에 모든 리소스를 생성하고, 파일(상태 파일)에서 기존 상태를 로드하고, 현재 클라우드 API에 대해 새로 고친 다음 차이점을 계산합니다. 차이점을 기반으로 Terraform은 인프라를 HCL 정의와 일치시키기 위한 일련의 생성, 수정 또는 삭제 작업인 "계획"을 제안합니다.

Terraform은 또한 리소스 간의 종속성을 자동으로 계산하고 올바른 생성/소멸 순서를 유지합니다. 실행 중 실패하면 전체 프로세스를 다시 시도할 수 있으며, 일반적으로 작업이 완료된 지점에서 다시 시작됩니다.

## more-learning

리소스를 소개할 시간입니다.

```terraform
variable "yaml-data" {

  // config는 .yaml 파일에서 가져오므로 기술적으로는 map(any)이지만 다음과 같이 유형을 좁힐 수 있습니다:
  type = map(string)
}

// 공급자를 명시적으로 정의할 필요는 없습니다. 모두 환경 변수가 있는 합리적인 기본값을 가지고 있습니다. 공급자에 의존하는 리소스를 사용하면 투명하게 초기화됩니다(terraform init을 호출할 때).
resource "aws_s3_bucket" "bucket" {
  bucket = "abc"
}

// 공급자 별칭을 만들 수도 있습니다.
provider "aws" {
  alias = "as-role"
  assume_role {
    role_arn = ".."
  }
}

// 그런 다음 리소스를 만드는 데 사용합니다.
resource "aws_s3_bucket_object" "test-file" {

  // 모든 리소스에는 참조할 수 있는 속성이 있습니다. 그 중 일부는 즉시 사용할 수 있으며(예: 버킷) 다른 일부는 계획이 실행되기 시작한 후에만 사용할 수 있습니다. test-file 리소스는 aws_s3_bucket.bucket 생성이 완료된 후에만 생성됩니다.

  // depends_on = aws_s3_bucket.bucket
  bucket = aws_s3_bucket.bucket.bucket
  key = "index.html"
  content = file("${path.module}/index.html")

  // 공급자 별칭을 수동으로 지정할 수도 있습니다.
  provider = aws.as-role
}

// 각 리소스는 상태에서 "aws_s3_bucket.bucket"과 같은 ID를 받습니다.
// 모듈 내에서 리소스가 생성되면 상태 ID 앞에 module.<module-name>이 붙습니다.

module "learn-each" {
  source = "../learn-each"
}

// 이와 같이 모듈을 중첩하는 것은 최상의 방법이 아닐 수 있으며, 여기서는 설명 목적으로만 사용됩니다.
```

## learn-each

Terraform은 일련의 개체를 만드는 데 유용한 몇 가지 기능을 제공합니다:

```terraform
locals {
  list = ["red", "green", "blue"]
}
resource "aws_s3_bucket" "badly-coloured-bucket" {
  count = count(local.list)
  bucket_prefix = "${local.list[count.index]}-"
}
// "red-" 등으로 접두사가 붙고 고유 식별자가 뒤따르는 3개의 버킷을 만듭니다. 일부 리소스는 지정되지 않은 경우 자동으로 임의의 이름을 생성합니다. 리소스(또는 이 예에서는 버킷)의 실제 이름은 속성으로 참조할 수 있습니다.

output "red-bucket-name" {
  value = aws_s3_bucket.badly-coloured-bucket[0].bucket
}

// 버킷 리소스 ID는 "aws_s3_bucket.badly-coloured-bucket[0]"에서 2까지입니다. 목록 인덱스 요소이기 때문입니다. 그러나 목록에서 "red"를 제거하면 이제 새 ID를 갖게 되므로 모든 버킷을 다시 만듭니다. 더 나은 방법은 for_each를 사용하는 것입니다.

resource "aws_s3_bucket" "coloured-bucket" {
  // for_each는 맵과 집합만 지원합니다.
  for_each = toset(local.list)
  bucket_prefix = "${each.value}-"
}

// 이 리소스의 이름은 aws_s3_bucket.coloured-bucket[red]입니다.

output "red-bucket-name2" {
  value = aws_s3_bucket.badly-coloured-bucket["red"].bucket
}

output "all-bucket-names" {

  // 버킷 이름이 포함된 목록을 반환합니다 - "스플랫 표현식" 사용
  value = aws_s3_bucket.coloured-bucket[*].bucket
}

// 다른 스플랫 표현식이 있습니다:
output "all-bucket-names2" {
  value = [for b in aws_s3_bucket.coloured-bucket: b.bucket]
}
// 필터를 포함할 수도 있습니다.
output "filtered-bucket-names" {
  value = [for b in aws_s3_bucket.coloured-bucket:
    b.bucket if length(b.bucket) < 10 ]
}

// 다음은 맵을 생성하는 몇 가지 방법입니다. {red: "red-123123.."}
output "bucket-map" {
  value = {
    for b in aws_s3_bucket.coloured-bucket:
       trimsuffix(b.bucket_prefix, '-')
         => b.bucket
   }
}

// Terraform 0.13부터는 모듈에 count/each를 사용할 수도 있습니다.

variable "learn-functions" {
  type = bool
  default = true
}

module "learn-functions" {
  count = var.learn-functions ? 1: 0
  source = "../learn-functions"
}
```

이것은 이제 Terraform 0.13에서 작동하는 인기 있는 구문으로, 모듈을 조건부로 포함할 수 있습니다.

## learn-functions

Terraform은 자신만의 함수를 정의할 수 없지만, 광범위한 내장 함수 목록이 있습니다.

```terraform
locals {
  list = ["one", "two", "three"]

  upper_list = [for x in local.list : upper(x) ] // "ONE", "TWO", "THREE"

  map = {for x in local.list : x => upper(x) } // "one":"ONE", "two":"TWO", "three":"THREE"

  filtered_list = [for k, v in local.map : substr(v, 0, 2) if k != "two" ] // "ON", "TH"

  prefixed_list = [for v in local.filtered_list : "pre-${v}" ] // "pre-ON", "pre-TH"

  joined_list = join(local.upper_list,local. filtered_list) // "ONE", "TWO", "THREE", "pre-ON", "pre-TH"

  // Set은 List와 매우 유사하지만 요소 순서는 관련이 없습니다.
  joined_set = toset(local.joined_list) // "ONE", "TWO", "THREE", "pre-ON", "pre-TH"

  map_again = map(slice(local.joined_list, 0, 4)) // "ONE":"TWO", "THREE":"pre-ON"
}

// 일반적으로 목록 조작은 for_each가 있는 리소스 또는 리소스에 대한 동적 블록을 지정하는 데 유용할 수 있습니다. 이것은 일부 태그가 있는 버킷을 만듭니다:

resource "aws_s3_bucket" "bucket" {
  name = "test-bucket"
  tags = local.map_again
}

// 이것은 다음과 동일합니다:
// resource "aws_s3_bucket" "bucket" {
//   name = "test-bucket"
//   tags = {
//     ONE = "TWO"
//     THREE = "pre-ON"
//   }
// }

// 일부 리소스에는 동적 블록도 포함됩니다. 다음 예제에서는 "data" 블록을 사용하여 3개의 버킷(빨강, 녹색 및 파랑)을 조회한 다음 빨강 및 녹색 버킷에 대한 읽기 전용 액세스와 파랑 버킷에 대한 전체 액세스를 포함하는 정책을 만듭니다.

locals {
  buckets = {
    red = "read-only"
    green = "read-only"
    blue = "full"
  }
  // 파일에서 버킷을 로드할 수 있습니다:
  // bucket = file('bucket.json')

  actions = {
    "read-only" = ["s3:GetObject", "s3:GetObjectVersion"],
    "full" = ["s3:GetObject", "s3:GetObjectVersion", "s3:PutObject", "s3:PutObjectVersion"]
  }
  // 작업을 반복하지 않도록 작업을 조회합니다.
}

// 함수를 사용하여 맵 키를 집합으로 변환
data "aws_s3_bucket" "bucket" {
  for_each = toset(keys(local.buckets))
  bucket = each.value
}

// 정책에 대한 json 생성
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

// 그리고 이것은 실제로 모든 버킷에 대한 권한이 있는 AWS 정책을 만듭니다.
resource "aws_iam_policy" "policy" {
  policy = data.aws_iam_policy_document.role_policy.json
}
```

## 추가 자료

- [Terraform 팁 및 요령](https://blog.gruntwork.io/terraform-tips-tricks-loops-if-statements-and-gotchas-f739bbae55f9)
- [Terraform 표현식 및 함수를 사용한 동적 출력 빌드](https://www.thegreatcodeadventure.com/building-dynamic-outputs-with-terraform-for_each-for-and-zipmap/)