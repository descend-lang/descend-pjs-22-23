use crate::parser::SourceCode;
use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use std::fmt::Formatter;

#[must_use]
pub struct FileIOError<'a> {
    file_path: &'a str,
    io_error: std::io::Error,
}

impl<'a> FileIOError<'a> {
    pub fn new(file_path: &'a str, io_error: std::io::Error) -> Self {
        FileIOError {
            file_path,
            io_error,
        }
    }

    pub fn emit(&self) -> ErrorReported {
        println!("{}", self.to_string(self.file_path, &self.io_error));
        ErrorReported
    }

    fn to_string(&self, path_to_file: &str, err: &std::io::Error) -> String {
        let label = format!("couldn't read {}: {}", path_to_file, err);
        let snippet = Snippet {
            title: Some(Annotation {
                id: None,
                label: Some(&label),
                annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: vec![],
            opt: default_format(),
        };
        DisplayList::from(snippet).to_string()
    }
}

impl std::fmt::Debug for FileIOError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.io_error)
    }
}

pub fn default_format() -> FormatOptions {
    FormatOptions {
        color: true,
        anonymized_line_numbers: false,
        margin: None,
    }
}

pub fn single_line_snippet<'a>(
    source: &'a SourceCode<'a>,
    label: &'a str,
    line_num: usize,
    begin_column: usize,
    end_column: usize,
) -> Snippet<'a> {
    let line = source.get_line(line_num);
    Snippet {
        title: Some(Annotation {
            id: None,
            label: Some(label),
            annotation_type: AnnotationType::Error,
        }),
        footer: vec![],
        slices: vec![Slice {
            source: line,
            line_start: line_num + 1,
            origin: source.file_path,
            annotations: vec![SourceAnnotation {
                range: (begin_column, end_column),
                label,
                annotation_type: AnnotationType::Error,
            }],
            fold: false,
        }],
        opt: default_format(),
    }
}

pub struct ErrorReported;

impl std::fmt::Debug for ErrorReported {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Aborting due to previous error.")
    }
}
