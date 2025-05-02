//netlify-build.sh

#!/bin/bash

# Check if this is a preview build
if [ "$CONTEXT" == "deploy-preview" ] || [ "$CONTEXT" == "branch-deploy" ]; then
echo "Running incremental build for preview..."
quarto render --to html --execute-daemon
else
  echo "Running full build for production..."
quarto render
fi