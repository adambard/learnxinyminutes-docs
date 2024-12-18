#!/usr/bin/env python3

import re
from pathlib import Path
import yaml
import yamllint.config
import yamllint.linter


def extract_yaml_frontmatter(file_path):
    """Extracts YAML front matter from a Markdown file."""
    with open(file_path, "r", encoding="utf-8") as file:
        content = file.read()
        matches = re.match(r"^(---\s*\n.*?\n)---\n", content, re.DOTALL)
        if matches:
            return matches.group(1)
        return None


yaml_config = yamllint.config.YamlLintConfig(
    """{
        extends: default,
        rules: {
            commas: disable,
            trailing-spaces: disable,
            indentation: disable,
            line-length: disable,
            empty-lines: disable
        }
    }"""
)


def lint_yaml(yaml_content):
    """Lints YAML content using yamllint by sending it to stdin."""
    problems = []
    for p in yamllint.linter.run(yaml_content, yaml_config):
        problems.append(f"{p.line}:{p.column}    {p.desc} ({p.rule})")
    return "\n".join(problems)


def validate_yaml_keys(yaml_content, allowed_keys):
    """Validates that the YAML content contains only the specified keys."""
    try:
        data = yaml.safe_load(yaml_content)
        if not data:
            return "Empty YAML front matter."
        extra_keys = set(data.keys()) - set(allowed_keys)
        if extra_keys:
            return f"Invalid keys found: {', '.join(extra_keys)}"
        for key, value_type in allowed_keys.items():
            if key in data:
                if not isinstance(data[key], value_type):
                    return f"Invalid type for key '{key}': expected {value_type.__name__}, got {type(data[key]).__name__}"
                if isinstance(data[key], list):
                    for item in data[key]:
                        if not isinstance(item, list):
                            return f"Invalid type for item in key '{key}': expected list, got {type(item).__name__}"
                        elif not item:
                            return f"Invalid item in key '{key}': found empty list"
                        elif not isinstance(item[0], str):
                            return f"Invalid type for item[0] in key '{key}': expected str, got {type(item[0]).__name__}"
                        elif len(item) == 2 and not isinstance(item[1], str):
                            return f"Invalid type for item[1] in key '{key}': expected str, got {type(item[1]).__name__}"
                        elif len(item) > 2:
                            return f"Invalid length for item in key '{key}': expected 1 or 2, got {len(item)}"
    except yaml.YAMLError as e:
        return f"Error parsing YAML: {e}"
    return ""


def process_files(path):
    """Processes either a single file or all Markdown files in a directory."""
    if path.is_dir():
        pathlist = path.rglob("*.md")
    else:
        pathlist = [path]

    has_error = False
    allowed_keys = {
        "name": str,
        "where_x_eq_name": str,
        "category": str,
        "filename": str,
        "contributors": list,
        "translators": list,
    }
    for path in pathlist:
        yaml_content = extract_yaml_frontmatter(path)
        if yaml_content:
            lint_result = lint_yaml(yaml_content)
            key_validation = validate_yaml_keys(yaml_content, allowed_keys)
            if lint_result or key_validation:
                if has_error:  # don't prepend newline to first error
                    print()
                print(path)
                if lint_result:
                    print(lint_result)
                if key_validation:
                    print(key_validation)
                has_error = True
    return has_error


def main(path_input):
    """Determines if the input is a directory or a file and processes accordingly."""
    path = Path(path_input)
    if not path.exists():
        print(f"Error: {path_input} does not exist.")
        return 1

    return process_files(path)


if __name__ == "__main__":
    import sys

    path_input = sys.argv[1] if len(sys.argv) > 1 else "."
    has_error = main(path_input)
    sys.exit(1 if has_error else 0)
