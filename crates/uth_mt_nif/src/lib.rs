use rustler::Atom;

mod atoms {
    rustler::atoms! {
        ok,
        pong,
        invalid_pitch,
    }
}

#[rustler::nif]
fn pong() -> Atom {
    atoms::pong()
}

#[rustler::nif(name = "parse-midi-pitch")]
fn parse_midi_pitch(s: String) -> Result<(Atom, u8), rustler::Error> {
    music_comp_mt::note::parse_midi_pitch(&s)
        .map(|n| (atoms::ok(), n))
        .map_err(|_| rustler::Error::Term(Box::new(atoms::invalid_pitch())))
}

rustler::init!("uth.mt.nif");
