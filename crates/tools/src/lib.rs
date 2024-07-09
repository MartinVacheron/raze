pub mod results;


pub trait ToUuid {
    fn to_uuid(&self) -> String;
}