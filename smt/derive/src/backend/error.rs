/// An error for backend generator
pub enum BackendError {
    NotSupported,
}

pub type BackendResult<T> = Result<T, BackendError>;
