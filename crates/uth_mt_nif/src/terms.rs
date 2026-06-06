use crate::convert::pitch_to_atom;
use music_comp_mt::note::Note;
use music_comp_mt::quintal::PcChord;
use rustler::{Atom, NifMap};

/// Erlang-term shape for a note: `#{pitch => atom(), octave => integer()}`.
#[derive(NifMap)]
pub struct NoteMap {
    pub pitch: Atom,
    pub octave: u8,
}

impl NoteMap {
    pub fn from_note(note: Note) -> Self {
        NoteMap {
            pitch: pitch_to_atom(note.pitch),
            octave: note.octave,
        }
    }
}

/// Erlang-term shape for a quintal pitch-class chord: `#{pcs => [integer()]}`.
/// `PcChord.pcs` is `[u8; 4]`; encoded as a list for term-encoding ergonomics.
#[derive(NifMap)]
pub struct PcChordTerm {
    pub pcs: Vec<u8>,
}

impl From<PcChord> for PcChordTerm {
    fn from(pc: PcChord) -> Self {
        PcChordTerm {
            pcs: pc.pcs.to_vec(),
        }
    }
}
