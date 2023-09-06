#!/bin/bash

# Step 1: Compile cobol_tetris.cob
cobc -x cobol_tetris.cob

# Step 2: Compile DRAW_BOARD.cob
cobc -m DRAW_BOARD.cob

# Step 3: Compile CONVERT_TIME_TO_SEC.cob
cobc -m CONVERT_TIME_TO_SEC.cob

# Step 4: Compile MAKE_BOARDER.cob
cobc -m MAKE_BORDER.cob

# Prompt the user for input
read -p "Do you want to run ./cobol_tetris? (y/n): " choice

# Check if the user input is 'y' or 'Y'
if [[ "$choice" =~ ^[Yy]$ ]]; then
    ./cobol_tetris
else
    echo "Compile Complete."
fi

