# diffparser
Tool to convert the output of `git diff` to JSON for further processing.

## Example

Consider a `git diff` output looking as such:

```diff
diff --git a/factorial.py b/factorial.py
index abcdef1..1234567 100644
--- a/factorial.py
+++ b/factorial.py
@@ -1,6 +1,7 @@
 def factorial(n):
+    # Calculate the factorial of a number recursively
     if n == 0:
         return 1
     else:
-        return n * factorial(n - 1)
+        return n * factorial(n - 1)
 
-num = 5
+num = 6
 print(f"The factorial of {num} is {factorial(num)}")
```

We can pipe this output to `diffparser` binary and `jq` for formatting:

```sh
git diff | diffparser | jq
```

Output:

```json
[
  {
    "oldName": "factorial.py",
    "newName": "factorial.py",
    "index": "index abcdef1..1234567 100644",
    "hunks": [
      {
        "oldRange": {
          "from": 1,
          "range": 6
        },
        "newRange": {
          "from": 1,
          "range": 7
        },
        "lines": [
          {
            "type": "context",
            "line": "def factorial(n):"
          },
          {
            "type": "added",
            "line": "    # Calculate the factorial of a number recursively"
          },
          {
            "type": "context",
            "line": "    if n == 0:"
          },
          {
            "type": "context",
            "line": "        return 1"
          },
          {
            "type": "context",
            "line": "    else:"
          },
          {
            "type": "removed",
            "line": "        return n * factorial(n - 1)"
          },
          {
            "type": "added",
            "line": "        return n * factorial(n - 1)"
          },
          {
            "type": "context",
            "line": ""
          },
          {
            "type": "removed",
            "line": "num = 5"
          },
          {
            "type": "added",
            "line": "num = 6"
          },
          {
            "type": "context",
            "line": "print(f\"The factorial of {num} is {factorial(num)}\")"
          }
        ]
      }
    ]
  }
]
```
