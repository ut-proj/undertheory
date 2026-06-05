use rustler::Atom;

mod atoms {
    rustler::atoms! { pong }
}

#[rustler::nif]
fn pong() -> Atom {
    atoms::pong()
}

rustler::init!("uth.mt.nif");
