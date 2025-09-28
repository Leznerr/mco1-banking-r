# R/formatters.R                                                              
# Purpose:                                                                     
# - Provide a consistent "money" formatter (2 decimals; no scientific notation)
# - Print neat section headings and horizontal rules in the console
# - Offer tiny column formatters for aligned tables (optional helpers)
# Policy:                                                                      
# - Do NOT perform business/math here—only convert numbers to display strings.  

# ─────────────────────────────────────────────────────────────────────────────
# money(x, digits = 2, na = "NA")
# Purpose : Convert numeric vector x to character with fixed decimals.
# Inputs  : x (numeric vector), digits (# of decimals), na (string for NA/Inf)
# Returns : character vector the same length as x.
# Notes   : No currency symbol is added; the UI concatenates " PHP"/" USD" etc.
# ─────────────────────────────────────────────────────────────────────────────
money <- function(x, digits = 2, na = "NA") {
  if (!is.numeric(x)) {                                                        # Guard: ensure x is numeric; fail early for clarity.
    stop("money(): 'x' must be numeric.", call. = FALSE)
  }
  # formatC produces fixed decimal strings, rounding as needed and never using
  # scientific notation (format = 'f'). This preserves a consistent screen UX.
  out <- formatC(x, format = "f", digits = digits)                             # Core conversion → character strings at fixed precision.
  out[!is.finite(x)] <- na                                                     # Replace NA/Inf/-Inf with a friendly placeholder string.
  out                                                                          # Return the character vector for UI printing.
}

# ─────────────────────────────────────────────────────────────────────────────
# heading(text, width = 30, char = "=")
# Purpose : Print a simple section heading with underlines for readability.
# Inputs  : text (string), width (underline length), char (underline character)
# Returns : (invisible) NULL after printing to console.
# ─────────────────────────────────────────────────────────────────────────────
heading <- function(text, width = 30, char = "=") {
  if (!is.character(text) || length(text) != 1) {                              # Guard: heading expects a single string.
    stop("heading(): 'text' must be a single character string.", call. = FALSE)
  }
  cat("\n")                                                                    # Blank line before header improves readability.
  cat(paste0(strrep(char, width), "\n"))                                       # Top rule (e.g., "==============================").
  cat(text, "\n", sep = "")                                                    # The actual heading text on its own line.
  cat(paste0(strrep(char, width), "\n\n"))                                     # Bottom rule + trailing blank line.
  invisible(NULL)                                                              # Explicit invisible return (console printer).
}

# ─────────────────────────────────────────────────────────────────────────────
# rule(width = 30, char = "-")
# Purpose : Print a horizontal rule (useful to separate sections).
# Inputs  : width (characters), char (the character to repeat)
# Returns : (invisible) NULL after printing to console.
# ─────────────────────────────────────────────────────────────────────────────
rule <- function(width = 30, char = "-") {
  cat(paste0(strrep(char, width), "\n"))                                       # Print the repeated character followed by newline.
  invisible(NULL)                                                              # Console helper; return value is not used.
}

# ─────────────────────────────────────────────────────────────────────────────
# Optional column helpers for aligned tables in plain console (ASCII only)
# These are NOT required by the spec but help produce neat interest tables.
# The UI may keep using sprintf() directly; these are available if you prefer.
# ─────────────────────────────────────────────────────────────────────────────

# pad_left(s, width)
# Purpose : Left-pad string s with spaces up to 'width' (right-align numbers).
pad_left <- function(s, width) {
  s <- as.character(s)                                                         # Ensure 's' is character for padding operations.
  n <- nchar(s, type = "width")                                                # Visual width in monospace console (ASCII-safe).
  p <- pmax(0, width - n)                                                      # Number of spaces to add (non-negative).
  paste0(strrep(" ", p), s)                                                    # Prepend spaces to right-align within the column.
}

# pad_right(s, width)
# Purpose : Right-pad string s with spaces up to 'width' (left-align text).
pad_right <- function(s, width) {
  s <- as.character(s)                                                         # Coerce to character to avoid recycling issues.
  n <- nchar(s, type = "width")                                                # Compute display width.
  p <- pmax(0, width - n)                                                      # Spaces to append to reach desired width.
  paste0(s, strrep(" ", p))                                                    # Append spaces to left-align within the column.
}

# fmt_num(x, width = 10, digits = 2, na = "NA")
# Purpose : Right-align a numeric column at fixed decimals (uses money()).
fmt_num <- function(x, width = 10, digits = 2, na = "NA") {
  pad_left(money(x, digits = digits, na = na), width)                          # Format as money → pad left to align numerically.
}

# fmt_text(s, width = 10, align = c("left", "right", "center"))
# Purpose : Align plain text within a fixed-width column.
fmt_text <- function(s, width = 10, align = c("left", "right", "center")) {
  align <- match.arg(align)                                                    # Accept only one of the three supported alignments.
  s <- as.character(s)                                                         # Coerce to character vector.
  if (align == "left")   return(pad_right(s, width))                           # Left alignment: pad on the right.
  if (align == "right")  return(pad_left(s,  width))                           # Right alignment: pad on the left.
  # Center alignment: split padding on both sides (favor left when odd).
  n <- nchar(s, type = "width")                                                # Compute current width of strings.
  total_pad <- pmax(0, width - n)                                              # Total padding required for each element.
  left_pad  <- floor(total_pad / 2)                                            # Left side spaces.
  right_pad <- total_pad - left_pad                                            # Right side spaces (complements left).
  paste0(strrep(" ", left_pad), s, strrep(" ", right_pad))                     # Surround s with spaces to center-align.
}
