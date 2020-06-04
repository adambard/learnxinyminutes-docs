---
category: tool
tool: AWS CloudFormation
contributors:
    - ["Pat Myron", "http://github.com/patmyron"]
---

[AWS CloudFormation](https://aws.amazon.com/cloudformation/) provides a language to describe and provision cloud infrastructure resources.

The [AWS CloudFormation Linter](https://github.com/aws-cloudformation/cfn-python-lint) helps catch errors in templates before deploying them.

```yaml
# YAML comments supported

Parameters:  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html
  CreateResource:
    Default: false
    Type: String
    AllowedValues: [true, false]

Conditions:  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/conditions-section-structure.html
  ShouldCreateResource:
    !Equals [true, !Ref CreateResource]

Resources:  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resources-section-structure.html
  NeptuneDBCluster:
    Type: AWS::Neptune::DBCluster
    Condition: ShouldCreateResource            # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/conditions-section-structure.html
    DependsOn: ConfigAggregationAuthorization  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-dependson.html
    DeletionPolicy: Delete                     # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-deletionpolicy.html
    UpdateReplacePolicy: Delete                # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-updatereplacepolicy.html      
    Properties:
      Port: 99

  ConfigAggregationAuthorization:
    Type: AWS::Config::AggregationAuthorization
    Properties:
      AuthorizedAccountId: !Ref AWS::AccountId  # Pseudo parameters are parameters that are predefined by AWS CloudFormation
      AuthorizedAwsRegion: !Ref AWS::Region     # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/pseudo-parameter-reference.html

  # Not all property types are required. Some resource types may not require any properties
  # Property information can be found in both the documentation and the CloudFormation resource specification
  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html
  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cfn-resource-specification.html
  S3Bucket:
    Type: AWS::S3::Bucket

Outputs:  # https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/outputs-section-structure.html
  Output:
    Value: !Ref S3Bucket
```
