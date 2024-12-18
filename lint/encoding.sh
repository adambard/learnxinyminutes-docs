#!/bin/bash

check_encoding() {
    file="$1"
    encoding=$(file -b --mime-encoding "$file")

    # Check if the encoding is neither UTF-8 nor US-ASCII
    if [[ "$encoding" != "utf-8" && "$encoding" != "us-ascii" ]]; then
        # Print the file path and encoding
        echo "Error: $file has encoding $encoding, which is not utf-8 or us-ascii"
        return 1
    fi

    # Check for UTF-8 BOM
    if [[ "$encoding" == "utf-8" ]]; then
        if head -c 3 "$file" | cmp -s <(echo -ne '\xEF\xBB\xBF'); then
            echo "Error: $file contains a UTF-8 BOM"
            return 1
        fi
    fi

    return 0
}

export -f check_encoding

# Default to current directory if no argument is given
directory="${1:-.}"

find "$directory" -type f -name "*.md" -print0 | xargs -0 -P 8 -I {} bash -c 'check_encoding "$@"' _ {}
