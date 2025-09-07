#!/usr/bin/env python3
import argparse
import os
import re
import sys

TEST_START_RE = re.compile(r'\btest\b[^{]*{', re.MULTILINE)

def strip_test_blocks(text: str) -> str:
    """
    Remove Zig `test ... { ... }` blocks (inclusive of the braces).
    Handles nested braces within the test body and basic string literals.
    This is a pragmatic scanner for this specific use-case.
    """
    out_parts = []
    i = 0
    n = len(text)

    while i < n:
        m = TEST_START_RE.search(text, i)
        if not m:
            out_parts.append(text[i:])
            break

        start = m.start()
        brace_open = m.end() - 1  # index of the '{' matched by the regex

        # Keep everything before the `test ... {`
        out_parts.append(text[i:start])

        # Skip the balanced block starting at brace_open
        depth = 1
        j = brace_open + 1
        in_string = False
        escape = False

        while j < n and depth > 0:
            ch = text[j]

            if in_string:
                if escape:
                    escape = False
                elif ch == '\\':
                    escape = True
                elif ch == '"':
                    in_string = False
            else:
                if ch == '"':
                    in_string = True
                elif ch == '{':
                    depth += 1
                elif ch == '}':
                    depth -= 1

            j += 1

        # Continue scanning after the closing brace (or EOF)
        i = j

    return ''.join(out_parts)

def count_lines(s: str) -> int:
    """
    Count lines similar to wc: number of newline characters plus
    one if the text is non-empty and doesn't end with a newline.
    """
    if not s:
        return 0
    nl = s.count('\n')
    return nl if s.endswith('\n') else nl + 1

def should_skip_path_component(name: str) -> bool:
    return name.startswith('.')  # hidden on Linux

def file_should_be_included(path: str, include_tests: bool) -> bool:
    base = os.path.basename(path)
    if should_skip_path_component(base):
        return False
    if not base.endswith('.zig'):
        return False
    if not include_tests and ('test' in base):
        return False
    return True

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Count lines in .zig files recursively (similar to wc)."
    )
    parser.add_argument(
        "--include-tests",
        action="store_true",
        help="Include test files and test blocks (default: exclude).",
    )
    args = parser.parse_args()

    total = 0
    any_output = False
    root = os.getcwd()

    for dirpath, dirnames, filenames in os.walk(root, topdown=True):
        # Prune hidden directories
        dirnames[:] = [d for d in dirnames if not should_skip_path_component(d)]

        # Optionally prune directories that contain 'test'
        if not args.include_tests:
            dirnames[:] = [d for d in dirnames if 'test' not in d]

        for fname in filenames:
            if should_skip_path_component(fname):
                continue

            fpath = os.path.join(dirpath, fname)
            if not file_should_be_included(fpath, args.include_tests):
                continue

            try:
                with open(fpath, 'r', encoding='utf-8') as f:
                    text = f.read()
            except (OSError, UnicodeDecodeError):
                # Skip unreadable files
                continue

            if args.include_tests:
                content_for_count = text
            else:
                content_for_count = strip_test_blocks(text)

            nlines = count_lines(content_for_count)
            rel = os.path.relpath(fpath, root)
            print(f"{nlines}\t{rel}")
            total += nlines
            any_output = True

    # Print total like wc
    print(f"{total}\ttotal" if any_output else f"0\ttotal")

if __name__ == "__main__":
    sys.exit(main())
