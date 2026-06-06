use crate::atoms;
use music_comp_mt::chord::{Number, Quality};
use music_comp_mt::note::{NoteLetter, Pitch, PitchSymbol};
use rustler::Atom;

/// LFE pitch atom -> mt-rs `PitchSymbol`. Errors with `unknown-pitch`.
pub fn atom_to_pitch(atom: Atom) -> Result<PitchSymbol, Atom> {
    use PitchSymbol as P;
    match atom {
        a if a == atoms::atom_bs() => Ok(P::Bs),
        a if a == atoms::atom_c() => Ok(P::C),
        a if a == atoms::atom_cs() => Ok(P::Cs),
        a if a == atoms::atom_db() => Ok(P::Db),
        a if a == atoms::atom_d() => Ok(P::D),
        a if a == atoms::atom_ds() => Ok(P::Ds),
        a if a == atoms::atom_eb() => Ok(P::Eb),
        a if a == atoms::atom_e() => Ok(P::E),
        a if a == atoms::atom_fb() => Ok(P::Fb),
        a if a == atoms::atom_es() => Ok(P::Es),
        a if a == atoms::atom_f() => Ok(P::F),
        a if a == atoms::atom_fs() => Ok(P::Fs),
        a if a == atoms::atom_gb() => Ok(P::Gb),
        a if a == atoms::atom_g() => Ok(P::G),
        a if a == atoms::atom_gs() => Ok(P::Gs),
        a if a == atoms::atom_ab() => Ok(P::Ab),
        a if a == atoms::atom_a() => Ok(P::A),
        a if a == atoms::atom_as() => Ok(P::As),
        a if a == atoms::atom_bb() => Ok(P::Bb),
        a if a == atoms::atom_b() => Ok(P::B),
        a if a == atoms::atom_cb() => Ok(P::Cb),
        _ => Err(atoms::unknown_pitch()),
    }
}

/// LFE quality atom -> mt-rs `Quality`. Errors with `unknown-quality`.
pub fn atom_to_quality(atom: Atom) -> Result<Quality, Atom> {
    use Quality as Q;
    match atom {
        a if a == atoms::major() => Ok(Q::Major),
        a if a == atoms::minor() => Ok(Q::Minor),
        a if a == atoms::diminished() => Ok(Q::Diminished),
        a if a == atoms::augmented() => Ok(Q::Augmented),
        a if a == atoms::half_diminished() => Ok(Q::HalfDiminished),
        a if a == atoms::dominant() => Ok(Q::Dominant),
        a if a == atoms::suspended2() => Ok(Q::Suspended2),
        a if a == atoms::suspended4() => Ok(Q::Suspended4),
        _ => Err(atoms::unknown_quality()),
    }
}

/// LFE number atom -> mt-rs `Number`. Errors with `unknown-number`.
pub fn atom_to_number(atom: Atom) -> Result<Number, Atom> {
    use Number as N;
    match atom {
        a if a == atoms::triad() => Ok(N::Triad),
        a if a == atoms::seventh() => Ok(N::Seventh),
        a if a == atoms::major_seventh() => Ok(N::MajorSeventh),
        a if a == atoms::ninth() => Ok(N::Ninth),
        a if a == atoms::eleventh() => Ok(N::Eleventh),
        a if a == atoms::thirteenth() => Ok(N::Thirteenth),
        _ => Err(atoms::unknown_number()),
    }
}

/// mt-rs `Pitch` (letter + accidental) -> LFE pitch atom. This inverts
/// mt-rs's `From<PitchSymbol> for Pitch` table over its 21 spellings; any
/// pitch outside that table (e.g. a double accidental) falls back to
/// `unknown-pitch` (chord triads never produce those).
pub fn pitch_to_atom(pitch: Pitch) -> Atom {
    use NoteLetter::*;
    match (pitch.letter, pitch.accidental) {
        (C, 0) => atoms::atom_c(),
        (C, 1) => atoms::atom_cs(),
        (C, -1) => atoms::atom_cb(),
        (D, -1) => atoms::atom_db(),
        (D, 0) => atoms::atom_d(),
        (D, 1) => atoms::atom_ds(),
        (E, -1) => atoms::atom_eb(),
        (E, 0) => atoms::atom_e(),
        (E, 1) => atoms::atom_es(),
        (F, -1) => atoms::atom_fb(),
        (F, 0) => atoms::atom_f(),
        (F, 1) => atoms::atom_fs(),
        (G, -1) => atoms::atom_gb(),
        (G, 0) => atoms::atom_g(),
        (G, 1) => atoms::atom_gs(),
        (A, -1) => atoms::atom_ab(),
        (A, 0) => atoms::atom_a(),
        (A, 1) => atoms::atom_as(),
        (B, -1) => atoms::atom_bb(),
        (B, 0) => atoms::atom_b(),
        (B, 1) => atoms::atom_bs(),
        _ => atoms::unknown_pitch(),
    }
}
