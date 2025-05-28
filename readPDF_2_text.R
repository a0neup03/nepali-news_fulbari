#install.packages("pdftools")
library(pdftools)


# Load the library
library(pdftools)

# --- How to use for multiple PDFs ---



# Special handling for non-standard Nepali font encodings
library(pdftools)
library(stringi)

# Function to convert text with traditional Nepali font encodings to Unicode
convert_nepali_encoding <- function(text_file, output_file = NULL) {
  # Set default output file
  if (is.null(output_file)) {
    output_file <- paste0(tools::file_path_sans_ext(text_file), "_unicode.txt")
  }
  
  # Read the text file
  lines <- readLines(text_file, warn = FALSE)
  
  # Conversion mapping for common Preeti/Kantipur font encodings to Unicode
  # This is a simplified version - a complete mapping would be much longer
  preeti_to_unicode <- list(
    # Vowels
    "c" = "अ", "cf" = "आ", "O{" = "ई", "O" = "इ", "p" = "उ", "pm" = "ऊ", 
    "C" = "ऋ", "P]" = "ऐ", "cf]" = "ओ", "cf}" = "औ", "c+" = "अं", "cM" = "अः",
    
    # Consonants
    "s" = "क", "v" = "ख", "u" = "ग", "3" = "घ", "ª" = "ङ",
    "r" = "च", "5" = "छ", "h" = "ज", "em" = "झ", "`" = "ञ",
    "6" = "ट", "7" = "ठ", "8" = "ड", "9" = "ढ", "0f" = "ण",
    "t" = "त", "y" = "थ", "b" = "द", "w" = "ध", "g" = "न",
    "k" = "प", "km" = "फ", "a" = "ब", "e" = "भ", "d" = "म",
    "o" = "य", "/" = "र", "n" = "ल", "j" = "व", "z" = "श",
    "if" = "ष", ";" = "स", "x" = "ह", "If" = "क्ष", "q" = "त्र", "1" = "ज्ञ",
    
    # Matras
    "f" = "ा", "l" = "ि", "L" = "ी", "'" = "ु", "\"" = "ू", 
    "]" = "े", "}" = "ै", "f]" = "ो", "f}" = "ौ", "+" = "ं", "M" = "ः",
    
    # Special characters
    "." = "।", "F" = "ॐ", "?" = "रु", "&" = "्र", "%" = "छ्य", "#" = "ट्ट",
    "¶" = "द्य", "¿" = "न्न", "±" = "त्त", "°" = "त्र"
  )
  
  # Function to convert a line of text using the mapping
  convert_line <- function(line) {
    # This is a simplified approach - a proper implementation would use
    # regular expressions and handle compound characters better
    
    # Try basic character replacement
    for (old_char in names(preeti_to_unicode)) {
      line <- gsub(old_char, preeti_to_unicode[[old_char]], line, fixed = TRUE)
    }
    
    return(line)
  }
  
  # Process each line
  converted_lines <- sapply(lines, convert_line)
  
  # Write the result with UTF-8 encoding
  con <- file(output_file, "wb")
  for (line in converted_lines) {
    writeBin(charToRaw(paste0(line, "\n")), con)
  }
  close(con)
  
  message("Converted text saved to: ", output_file)
  return(output_file)
}

# OCR approach for Nepali
improved_ocr_nepali <- function(pdf_file, output_file = NULL, dpi = 400) {
  # Check for tesseract
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    stop("Package 'tesseract' is required. Please install it.")
  }
  
  # Check if Nepali is available
  if (!"nep" %in% tesseract::tesseract_info()$available) {
    message("Downloading Nepali language pack...")
    tesseract::tesseract_download("nep")
  }
  
  # Set default output
  if (is.null(output_file)) {
    output_file <- paste0(tools::file_path_sans_ext(basename(pdf_file)), "_ocr.txt")
  }
  
  # Create temp directory for images
  temp_dir <- tempfile("pdf_images")
  dir.create(temp_dir)
  
  # Get PDF info
  pdf_info_data <- pdf_info(pdf_file)
  n_pages <- pdf_info_data$pages
  
  # Convert PDF to high-resolution images (better for OCR)
  pdf_images <- character(n_pages)
  message("Converting PDF to high-resolution images...")
  for (i in 1:n_pages) {
    pdf_images[i] <- file.path(temp_dir, paste0("page_", i, ".png"))
    pdf_convert(pdf_file, format = "png", dpi = dpi, pages = i, 
                filenames = pdf_images[i])
  }
  
  # Create OCR engine with optimized settings for Nepali
  eng <- tesseract::tesseract("nep", options = list(
    preserve_interword_spaces = 1,  # Keep original spacing
    tessedit_pageseg_mode = 6,      # Assume uniform text block
    textord_heavy_nr = 1            # Better for non-Roman scripts
  ))
  
  # Perform OCR with higher accuracy
  message("Performing OCR with optimized settings...")
  con <- file(output_file, "wb")
  
  for (i in seq_along(pdf_images)) {
    message("Processing page ", i, "/", n_pages)
    # OCR the image
    ocr_text <- tesseract::ocr(pdf_images[i], engine = eng)
    
    # Add page marker and write to file
    page_marker <- paste0("\n\n--- Page ", i, " ---\n\n")
    ocr_text <- paste0(page_marker, ocr_text)
    
    # Write with UTF-8 encoding
    writeBin(charToRaw(ocr_text), con)
  }
  
  close(con)
  unlink(temp_dir, recursive = TRUE)
  
  message("OCR completed. Text saved to: ", output_file)
  return(output_file)
}

# Complete approach for Nepali PDF with both methods
extract_nepali_complete <- function(pdf_file, output_dir = ".") {
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Base name for output files
  base_name <- tools::file_path_sans_ext(basename(pdf_file))
  
  results <- list()
  
  # 1. Basic text extraction (may have encoding issues)
  text_file <- file.path(output_dir, paste0(base_name, "_text.txt"))
  tryCatch({
    # Extract text from PDF
    text <- pdf_text(pdf_file)
    
    # Save text
    con <- file(text_file, "wb")
    for (page in text) {
      writeBin(charToRaw(paste0(page, "\n\n")), con)
    }
    close(con)
    
    results$text_file <- text_file
    message("Basic text extraction saved to: ", text_file)
    
    # Try to convert encoding
    converted_file <- file.path(output_dir, paste0(base_name, "_converted.txt"))
    tryCatch({
      converted_file <- convert_nepali_encoding(text_file, converted_file)
      results$converted_file <- converted_file
    }, error = function(e) {
      warning("Font conversion failed: ", e$message)
    })
    
  }, error = function(e) {
    warning("Basic text extraction failed: ", e$message)
  })
  
  # 2. OCR approach (more reliable for Nepali)
  ocr_file <- file.path(output_dir, paste0(base_name, "_ocr.txt"))
  tryCatch({
    ocr_file <- improved_ocr_nepali(pdf_file, ocr_file)
    results$ocr_file <- ocr_file
  }, error = function(e) {
    warning("OCR extraction failed: ", e$message)
  })
  
  # 3. Extract tables from the best available text
  tables_dir <- file.path(output_dir, paste0(base_name, "_tables"))
  if (!dir.exists(tables_dir)) dir.create(tables_dir)
  
  source_for_tables <- if (!is.null(results$ocr_file)) {
    results$ocr_file
  } else if (!is.null(results$converted_file)) {
    results$converted_file
  } else if (!is.null(results$text_file)) {
    results$text_file
  } else {
    NULL
  }
  
  if (!is.null(source_for_tables)) {
    tryCatch({
      # Read the text
      text_content <- readLines(source_for_tables, warn = FALSE)
      
      # Simple table detection
      is_table_row <- logical(length(text_content))
      for (i in 1:length(text_content)) {
        line <- text_content[i]
        # Skip empty lines
        if (nchar(trimws(line)) < 5) next
        
        # Check for table-like patterns
        # 1. Multiple text blocks separated by spaces
        segments <- strsplit(trimws(line), "\\s{3,}")[[1]]
        if (length(segments) >= 2 && all(nchar(trimws(segments)) > 0)) {
          is_table_row[i] <- TRUE
          next
        }
        
        # 2. Consistently indented text
        if (grepl("^\\s{2,}\\S+.*\\s{2,}\\S+", line)) {
          is_table_row[i] <- TRUE
        }
      }
      
      # Group into tables
      if (sum(is_table_row) >= 2) {
        # Create runs of consecutive table rows
        runs <- rle(is_table_row)
        ends <- cumsum(runs$lengths)
        starts <- ends - runs$lengths + 1
        
        # Extract table sections
        tables <- list()
        for (i in which(runs$values)) {
          if (runs$lengths[i] >= 2) {  # Need at least 2 rows
            tables[[length(tables) + 1]] <- text_content[starts[i]:ends[i]]
          }
        }
        
        # Save tables
        if (length(tables) > 0) {
          for (i in seq_along(tables)) {
            # Save as text
            txt_file <- file.path(tables_dir, paste0("table_", i, ".txt"))
            con <- file(txt_file, "wb")
            for (line in tables[[i]]) {
              writeBin(charToRaw(paste0(line, "\n")), con)
            }
            close(con)
            
            # Save as CSV
            csv_file <- file.path(tables_dir, paste0("table_", i, ".csv"))
            
            # Split rows into columns
            rows <- lapply(tables[[i]], function(line) {
              parts <- strsplit(trimws(line), "\\s{3,}")[[1]]
              return(trimws(parts))
            })
            
            # Find max columns and pad rows
            max_cols <- max(sapply(rows, length))
            rows <- lapply(rows, function(row) {
              if (length(row) < max_cols) {
                c(row, rep("", max_cols - length(row)))
              } else {
                row
              }
            })
            
            # Create dataframe and save
            df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
            names(df) <- paste0("Col", 1:ncol(df))
            write.csv(df, file = csv_file, fileEncoding = "UTF-8", row.names = FALSE)
          }
          
          results$tables_dir <- tables_dir
          message("Extracted ", length(tables), " tables to: ", tables_dir)
        }
      }
    }, error = function(e) {
      warning("Table extraction failed: ", e$message)
    })
  }
  
  return(results)
}


# Specify the directory containing your PDF files
pdf_directory <- "/Users/neupanea2/Downloads/manifesto/" # Change this to your folder path

# List all PDF files in that directory
#pdf_files <- list.files(path = pdf_directory, pattern = "\\.pdf$", full.names = TRUE)


output_directory <- file.path(pdf_directory, "extracted_texts_R") # Subfolder for output

# Create output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# List all PDF files in that directory
pdf_files <- list.files(path = pdf_directory, pattern = "\\.pdf$", full.names = TRUE)






# 1. Complete processing:
 result1 <- process_nepali_pdf(pdf_file = pdf_files, output_directory)
 result1 <- process_nepali_pdf(pdf_file = pdf_files[2], output_directory)
 
# 2. Individual steps:
# text_file <- extract_pdf_text("path/to/file.pdf")
# tables_dir <- extract_pdf_tables("path/to/file.pdf")
# ocr_file <- ocr_pdf_nepali("path/to/file.pdf")
# clean_file <- clean_text_file("path/to/file_ocr.txt")