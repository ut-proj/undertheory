mod atoms;
mod convert;
mod resources;
mod terms;

use music_comp_mt::chord::Chord;
use music_comp_mt::note::Notes;
use music_comp_mt::quintal::{betweenness_centrality, BaseSpace};
use resources::BaseSpaceResource;
use rustler::{Atom, Error, ResourceArc};
use terms::{NoteMap, PcChordTerm};

#[rustler::nif]
fn ping() -> Atom {
    atoms::pong()
}

#[rustler::nif(name = "parse-midi-pitch")]
fn parse_midi_pitch(s: String) -> Result<(Atom, u8), Error> {
    music_comp_mt::note::parse_midi_pitch(&s)
        .map(|n| (atoms::ok(), n))
        .map_err(|_| Error::Term(Box::new(atoms::invalid_pitch())))
}

#[rustler::nif(name = "chord-notes")]
fn chord_notes(root: Atom, quality: Atom, number: Atom) -> Result<(Atom, Vec<NoteMap>), Error> {
    let p = convert::atom_to_pitch(root).map_err(|a| Error::Term(Box::new(a)))?;
    let q = convert::atom_to_quality(quality).map_err(|a| Error::Term(Box::new(a)))?;
    let n = convert::atom_to_number(number).map_err(|a| Error::Term(Box::new(a)))?;
    let chord = Chord::new(p.into(), q, n);
    let notes = chord.notes().into_iter().map(NoteMap::from_note).collect();
    Ok((atoms::ok(), notes))
}

#[rustler::nif(schedule = "DirtyCpu", name = "make-base-space")]
fn make_base_space() -> ResourceArc<BaseSpaceResource> {
    ResourceArc::new(BaseSpaceResource(BaseSpace::new()))
}

#[rustler::nif(schedule = "DirtyCpu", name = "betweenness-centrality")]
fn betweenness_centrality_nif(
    space: ResourceArc<BaseSpaceResource>,
) -> Vec<(PcChordTerm, f64)> {
    betweenness_centrality(&space.0)
        .into_iter()
        .map(|(pc, bc)| (pc.into(), bc))
        .collect()
}

rustler::init!("uth.mt.nif");
