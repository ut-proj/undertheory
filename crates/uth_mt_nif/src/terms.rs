use crate::convert::pitch_to_atom;
use music_comp_mt::note::Note;
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
