#!/usr/bin/env python

import copy
from enum import auto, Enum
import math
import re


class Config:
    NotesPerOctave = 12


class Chord:
    Types = ['M', 'm', 'o', '+']

    @staticmethod
    def gen_chords(do_include_inversions=False):
        for base in PitchClass.Gamut:
            for t in Chord.Types:
                chord = Chord(str(base) + t)
                yield chord
                if do_include_inversions:
                    yield chord.inversion1()
                    yield chord.inversion2()

    @staticmethod
    def print_neighboring_chords(max_dist=1):
        chords = list(Chord.gen_chords())
        for k1 in range(len(chords)):
            c1 = chords[k1]
            for k2 in range(k1):
                c2 = chords[k2]
                sd = c1.scalar_distance(c2)
                if sd <= max_dist:
                    print(f'Neighbors: {c1} <==> {c2}')

    @staticmethod
    def test_chord_construction(verbose=True):
        if verbose: print('== Test chord construction ==')

        c_maj = Chord('C')
        if verbose: print(f'C_Maj={c_maj}')
        assert(c_maj.vals() == [0, 4, 7])

        c_maj2 = Chord('CM')
        if verbose: print(f'C_Maj2={c_maj2}')
        assert(c_maj2.vals() == [0, 4, 7])

        c_min = Chord('Cm')
        if verbose: print(f'C min={c_min}')
        assert(c_min.vals() == [0, 3, 7])

        c_dim = Chord('Co')
        if verbose: print(f'C dim={c_dim}')
        assert(c_dim.vals() == [0, 3, 6])

        c_aug = Chord('C+')
        if verbose: print(f'C aug={c_aug}')
        assert(c_aug.vals() == [0, 4, 8])

    @staticmethod
    def test_unique(verbose=False):
        if verbose: print(f'== Test chord uniqueness ==')

        chords = set()
        for chord in Chord.gen_chords(do_include_inversions=False):
            chords.add(chord)
            if verbose:
                print(f'Chord {chord.name}: ' + '-'.join(map(str, chord.notes)))
        if verbose: print(f'INFO: Number of chords: {len(chords)}')
        assert(len(chords) == 48)

    # For now, only handles triads
    def __init__(self, name=''):
        """Construct chord from chord name.
        name = RootNote (base + flats + sharps), followed by chord type"""
        if name != '':
            self.name = name.strip()
            p = re.compile('(\w[bs]*)([mo+]?)')
            m =  p.match(self.name.strip())
            assert(m)
            # print(f'Number of groups={len(m.groups())}')
            g1 = m.group(1)
            g2 = m.group(2)
            root = PitchClass(g1)
            if g2 == '' or g2 == 'M':
                note2 = copy.copy(root).steps_up(4)
                note3 = copy.copy(note2).steps_up(3)
            elif g2 == 'm':
                note2 = copy.copy(root).steps_up(3)
                note3 = copy.copy(note2).steps_up(4)
            elif g2 == 'o':
                note2 = copy.copy(root).steps_up(3)
                note3 = copy.copy(note2).steps_up(3)
            elif g2 == '+':
                note2 = copy.copy(root).steps_up(4)
                note3 = copy.copy(note2).steps_up(4)
            else:
                raise ValueError(f'Unrecognized chord type: {g2}')
            self.notes = (root, note2, note3)

    def __copy__(self):
        chord = Chord()
        chord.name = self.name
        chord.notes = self.notes
        return chord

    def __eq__(self, other):
        return self.notes == other.notes

    def __hash__(self):
        return hash(self.notes)

    def __str__(self):
        return  f'{self.name}: ' + '-'.join(map(str, self.notes))

    def distance(self, other):
        return [self.notes[k].distance(other.notes[k]) for k in range(len(self.notes))]

    def scalar_distance(self, other):
        return sum(self.distance(other))

    # For now, only handles triads
    def inversion1(self):
        result = Chord()
        result.name = f'I1({self.name})'
        result.notes = (self.notes[1], self.notes[0], self.notes[2])
        return result

    # For now, only handles triads
    def inversion2(self):
        result = Chord()
        result.name = f'I2({self.name})'
        result.notes = (self.notes[2], self.notes[0], self.notes[1])
        return result

    def vals(self):
        return [note.val() for note in self.notes]

class PitchBase(Enum):
    C = 0
    # Cs = 1
    D = 2
    # Ds = 3
    E = 4
    F = 5
    # Fs = 6
    G = 7
    # Gs = 8
    A = 9
    # As = 10
    B = 11

    def next(pb):
        if pb == PitchBase.C: return PitchBase.D
        elif pb == PitchBase.D: return PitchBase.E
        elif pb == PitchBase.E: return PitchBase.F
        elif pb == PitchBase.F: return PitchBase.G
        elif pb == PitchBase.G: return PitchBase.A
        elif pb == PitchBase.A: return PitchBase.B
        elif pb == PitchBase.B: return PitchBase.C

    def prev(pb):
        if pb == PitchBase.C: return PitchBase.B
        elif pb == PitchBase.D: return PitchBase.C
        elif pb == PitchBase.E: return PitchBase.D
        elif pb == PitchBase.F: return PitchBase.E
        elif pb == PitchBase.G: return PitchBase.F
        elif pb == PitchBase.A: return PitchBase.G
        elif pb == PitchBase.B: return PitchBase.A


class PitchClass:
    @staticmethod
    def test_gamut(verbose=True):
        if verbose: print('== Test PitchClass values ==')

        bottom = PitchClass('C')
        notes_upward = [bottom.steps_up(k) for k in range(12)]
        vals_upward = [note.val() for note in notes_upward]
        if verbose:
            print('-'.join(map(str, notes_upward)))
        assert(min(vals_upward) == 0)
        assert(max(vals_upward) == 11)
        assert(len(set(vals_upward)) == 12)

        top = PitchClass('B')
        notes_downward = [top.steps_down(k) for k in range(12)]
        vals_downward = [note.val() for note in notes_downward]
        if verbose:
            print('-'.join(map(str, notes_downward)))
        assert(min(vals_downward) == 0)
        assert(max(vals_downward) == 11)
        assert(len(set(vals_downward)) == 12)

    def __init__(self, s=''):
        if s == '':
            return

        str2pb = {}
        for pb in PitchBase:
            pb_str = str(pb)
            ind = pb_str.index('.')
            key = pb_str[ind + 1:]
            # print(f'INFO: setting {key} -> {pb}')
            str2pb[key] = pb

        self.base = str2pb[s[0]]
        self.flats = 0
        self.sharps = 0
        for k in range(1, len(s)):
            if s[k] == 'b':
                self.flats += 1
            if s[k] == 's':
                self.sharps += 1
        if self.flats > 0 and self.sharps > 0:
            if self.flats > self.sharps:
                self.flats -= self.sharps
                self.sharps = 0
            elif self.flats < self.sharps:
                self.sharps -= self.flats
                self.flats = 0
            else:  # flats == sharps:
                self.flats = 0
                self.sharps = 0

    def __copy__(self):
        pc = PitchClass()
        pc.base = self.base
        pc.flats = self.flats
        pc.sharps = self.sharps
        # print('INFO: About to return from PitchClass.__copy__')
        return pc

    def __eq__(self, other):
        return self.base == other.base and self.flats == other.flats and self.sharps == other.sharps

    def __hash__(self):
        return hash((self.base, self.flats, self.sharps))

    def __str__(self):
        s = str(self.base)
        ind = s.index('.')
        pb_str = s[ind + 1:]
        return pb_str + 'b' * self.flats + 's' * self.sharps

    def add_flats(self, n):
        self.flats += n

    def add_sharps(self, n):
        self.sharps += n

    def distance(self, other):
        d = self.val() - other.val()
        d = d % Config.NotesPerOctave
        return min(d, Config.NotesPerOctave - d)

    def steps_down(self, n=0):
        assert(type(n).__name__ == 'int')
        if n == 0:
            return copy.copy(self)
        if n < 0:
            return self.steps_up(-n)
        pc = copy.copy(self)
        for _ in range(n):
            if pc.sharps > 0:
                pc.sharps -= 1
            elif pc.flats == 0:
                if pc.base == PitchBase.C:
                    pc.base = PitchBase.B
                elif pc.base == PitchBase.F:
                    pc.base = PitchBase.E
                else:
                    pc.flats += 1
            else:  # flats > 0
                if pc.base in [PitchBase.C, PitchBase.F]:
                    pass
                else:
                    pc.flats -= 1
                pc.base = PitchBase.prev(pc.base)
        return pc

    def steps_up(self, n=0):
        assert(type(n).__name__ == 'int')
        if n == 0:
            return copy.copy(self)
        if n < 0:
            return self.sub_semitones(-n)
        pc = copy.copy(self)
        for _ in range(n):
            if pc.flats > 0:
                pc.flats -= 1
            elif pc.sharps == 0:
                if pc.base == PitchBase.B:
                    pc.base = PitchBase.C
                elif pc.base == PitchBase.E:
                    pc.base = PitchBase.F
                else:
                    pc.sharps += 1
            else:  # sharps > 0
                if pc.base in [PitchBase.B, PitchBase.E]:
                    pass
                else:
                    pc.sharps -= 1
                pc.base = PitchBase.next(pc.base)
        return pc

    def val(self):
        return (self.base.value - self.flats + self.sharps) % Config.NotesPerOctave


PitchClass.Gamut = [
        PitchClass('C')
        , PitchClass('Cs')
        , PitchClass('D')
        , PitchClass('Ds')
        , PitchClass('E')
        , PitchClass('F')
        , PitchClass('Fs')
        , PitchClass('G')
        , PitchClass('Gs')
        , PitchClass('A')
        , PitchClass('As')
        , PitchClass('B')
        ]


def example_all():
    example_distances()
    print()
    example_inversions()
    print()
    example_neighbors()
    print()


def example_distances():
    print('== Show chord distances ==')
    c1 = Chord('C')
    c2 = Chord('Db+')
    print(f'c1={c1}')
    print(f'c2={c2}')
    print(f'distance={c1.distance(c2)}')


def example_inversions():
    print('== Show chord inversions ==')
    cM = Chord('C')
    print(f'{cM}')
    i1_cM = cM.inversion1()
    print(f'{i1_cM}')
    i2_cM = cM.inversion2()
    print(f'{i2_cM}')


def example_neighbors():
    print('== Show neighboring chords ==')
    Chord.print_neighboring_chords(1)


def main():
    example_all()

def test_all():
    Chord.test_chord_construction()
    print()
    Chord.test_unique()
    print()
    PitchClass.test_gamut()
    print()


if __name__ == '__main__':
    test_all()
    # example_all()
    # main()

