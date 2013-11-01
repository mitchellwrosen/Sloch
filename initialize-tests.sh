#!/bin/bash

# Create various testing files that cannot be tracked by Git.

touch "test/sloch/DirentTest/no-read-permission"
chmod a-r "test/sloch/DirentTest/no-read-permission"

mkdir "test/sloch/DirentTest/empty-directory"
