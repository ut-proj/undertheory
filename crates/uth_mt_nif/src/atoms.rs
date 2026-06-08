rustler::atoms! {
    ok,
    pong,

    // Error reasons. Each declares its user-facing hyphenated text directly so
    // the LFE wrappers stay pure passthroughs (F1 and F2 are consistent).
    invalid_pitch = "invalid-pitch",
    unknown_pitch = "unknown-pitch",
    unknown_quality = "unknown-quality",
    unknown_number = "unknown-number",

    // Pitch names. TL ruling (spec §10): single-letter accidental suffix,
    // `s` = sharp, `b` = flat, matching mt-rs's PitchSymbol variants.
    atom_bs = "Bs",
    atom_c  = "C",
    atom_cs = "Cs",
    atom_db = "Db",
    atom_d  = "D",
    atom_ds = "Ds",
    atom_eb = "Eb",
    atom_e  = "E",
    atom_fb = "Fb",
    atom_es = "Es",
    atom_f  = "F",
    atom_fs = "Fs",
    atom_gb = "Gb",
    atom_g  = "G",
    atom_gs = "Gs",
    atom_ab = "Ab",
    atom_a  = "A",
    atom_as = "As",
    atom_bb = "Bb",
    atom_b  = "B",
    atom_cb = "Cb",

    // Chord qualities (lowercase / hyphenated to match LFE atom idiom).
    major,
    minor,
    diminished,
    augmented,
    half_diminished = "half-diminished",
    dominant,
    suspended2,
    suspended4,

    // Chord numbers.
    triad,
    seventh,
    major_seventh = "major-seventh",
    ninth,
    eleventh,
    thirteenth,
}
