# Makefile for cobol_tetris

# Compiler options
COBC = cobc
COBFLAGS = -x

# Source files
COBOL_SOURCES = cobol_tetris.cob DRAW_BOARD.cob CONVERT_TIME_TO_SEC.cob MAKE_BORDER.cob CHECK_AND_CLEAR_ROWS.cob INITIALIZE_PIECES.cob GET_NEXT_PIECE.cob

# Output executable
EXECUTABLE = cobol_tetris

all: compile run

compile: $(COBOL_SOURCES)
	$(COBC) $(COBFLAGS) $(COBOL_SOURCES)

run:
	@read -p "Do you want to run ./$(EXECUTABLE)? (y/n): " choice; \
	if [ "$$choice" = "y" ] || [ "$$choice" = "Y" ]; then \
		./$(EXECUTABLE); \
	else \
		echo "Compile Complete."; \
	fi

clean:
	rm -f $(EXECUTABLE)
