use std::collections::BTreeSet;
use std::fmt;
use std::io::Write;

/// Describes the 3 kinds of Hanoi Towers.
#[derive(Debug, PartialEq, Clone, Hash)]
pub enum HanoiTower {
    T1,
    T2,
    T3,
}

impl HanoiTower {
    /// Number of Hanoi Tower
    pub const NUMBER_HANOI_TOWER: usize = 3;

    /// Get the number of the Tower in interval (1..3).
    pub fn number(&self) -> u8 {
        match self {
            HanoiTower::T1 => 1,
            HanoiTower::T2 => 2,
            HanoiTower::T3 => 3,
        }
    }

    /// Get an index starting at 0 from Hanoi Tower.
    pub fn index(&self) -> usize {
        match self {
            HanoiTower::T1 => 0,
            HanoiTower::T2 => 1,
            HanoiTower::T3 => 2,
        }
    }

    /// Takes 2 distinct towers t1 and t2 and return the last one.
    /// Will panic if t1 == t2.
    /// ```
    /// use annoying_tower_of_hanoi::HanoiTower;
    ///
    /// assert_eq!(HanoiTower::T1, HanoiTower::last_tower(HanoiTower::T2, HanoiTower::T3));
    /// assert_eq!(HanoiTower::T2, HanoiTower::last_tower(HanoiTower::T1, HanoiTower::T3));
    /// ```
    pub fn last_tower(t1: HanoiTower, t2: HanoiTower) -> HanoiTower {
        use HanoiTower::*;
        match (t1.clone(), t2.clone()) {
            (T1, T2) => T3,
            (T1, T3) => T2,
            (T2, T1) => T3,
            (T2, T3) => T1,
            (T3, T1) => T2,
            (T3, T2) => T1,
            _ => panic!("Invalid move from {} to {} to find_last_tower", t1, t2),
        }
    }
}

impl fmt::Display for HanoiTower {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "T{}", self.number())
    }
}

/// Type that can move Hanoi Towers.
/// In practice it's a type that can receive the tower moves.
pub trait HanoiDiscMover {
    /// Move disc from src HanoiTower to dest HanoiTower.
    fn move_disc(&mut self, src: HanoiTower, dest: HanoiTower);
}

type Discs =[Vec<usize>; HanoiTower::NUMBER_HANOI_TOWER];
/// Type that represents a mutable states of the disc on the Hanoi Towers.
pub trait HanoiDiscsState {


    /// Move the top disc from src HanoiTower to dest HanoiTower.
    fn move_disc(&mut self, src: HanoiTower, dest: HanoiTower);

    /// The number of Discs
    fn depth(&self) -> usize;

    /// Find the Hanoi Tower hosting the disc.
    /// Panic if disc_number >= depth.
    fn find_disc(&self, disc_number: usize) -> HanoiTower;

    /// Returns a Reference to all discs
    fn all_discs(&self) -> &Discs;
}

/// Store each disc move
#[derive(Debug, Default, PartialEq)]
pub struct DiscMoveStore(pub Vec<(HanoiTower, HanoiTower)>);

impl HanoiDiscMover for DiscMoveStore {
    fn move_disc(&mut self, src: HanoiTower, dest: HanoiTower) {
        self.0.push((src, dest));
    }
}

/// Solves the Hanoi Tower problem.
/// Use the provided mover each time a Disc is moved.
pub struct HanoiSolver<'a> {
    mover: &'a mut dyn HanoiDiscMover,
}

/// Write a step of the resolution of the HanoiTower
pub struct HanoiStepWriter<'a, T: Write> {
    // buffer: String,
    writer: &'a mut T,
}

impl<'a, T: Write> HanoiStepWriter<'a, T> {
    const HEADER_HEIGHT: usize = 3;
    const FOOTER_HEIGHT: usize = 3;
    const EXTRA_WIDTH: usize = 12;
    const WIDTH_DEPTH_COEFF: usize = 6;
    const CHAR_MAX_LENGTH_UTF8: usize = 3; //Longest char to draw are box drawing chars
    const DELIMITER: &'static str = "\n";

    pub fn build(writer: &'a mut T) -> HanoiStepWriter<'a, T> {
        // Set buffer max size to avoid realloc
        HanoiStepWriter {
            // buffer: String::with_capacity(
            //     display_area_chars * HanoiStepWriter::CHAR_MAX_LENGTH_UTF8,
            // ),
            writer,
        }
    }

    pub fn write_step(hanoi_discs: &HanoiTowersDiscs, step: usize) {
        unimplemented!()
    }

    pub fn write_move(src: HanoiTower, dest: HanoiTower, move_no: usize) {
        unimplemented!()
    }
}

impl<'a> HanoiSolver<'a> {
    pub fn build(mover: &mut dyn HanoiDiscMover) -> HanoiSolver {
        HanoiSolver { mover }
    }
    pub fn solve(&mut self, level: usize) {
        self.solve_recursive(HanoiTower::T1, HanoiTower::T3, HanoiTower::T2, level);
    }

    fn solve_recursive(
        &mut self,
        src: HanoiTower,
        dest: HanoiTower,
        proxy: HanoiTower,
        level: usize,
    ) {
        if level == 0 {
            return;
        }
        self.solve_recursive(src.clone(), proxy.clone(), dest.clone(), level - 1);
        self.mover.move_disc(src.clone(), dest.clone());
        self.solve_recursive(proxy, dest, src, level - 1);
    }
}

#[derive(Debug, PartialEq)]
pub struct HanoiTowersDiscs {
    discs: [Vec<usize>; HanoiTower::NUMBER_HANOI_TOWER],
    depth: usize,
}

#[derive(Debug, PartialEq)]
pub struct InvalidDiscsError {
    pub missing_values: Vec<usize>,
}

impl HanoiTowersDiscs {
    pub fn build(depth: usize) -> HanoiTowersDiscs {
        HanoiTowersDiscs {
            discs: [
                (0..depth).rev().collect(),
                Vec::with_capacity(depth),
                Vec::with_capacity(depth),
            ],
            depth,
        }
    }
    pub fn build_from_discs(discs: [Vec<usize>; HanoiTower::NUMBER_HANOI_TOWER]) -> Result<HanoiTowersDiscs, InvalidDiscsError> {
        let mut max_val: usize = 0;
        let all_values: BTreeSet<usize> = discs[0]
            .iter()
            .chain(discs[1].iter())
            .chain(discs[2].iter())
            .inspect(|x| {
                max_val = max_val.max(**x);
            })
            .cloned()
            .collect();
        let continuous_values: BTreeSet<usize> = (0..=max_val).collect();
        let missing_values: Vec<usize> =
            continuous_values.difference(&all_values).cloned().collect();
        if !missing_values.is_empty() {
            Err(InvalidDiscsError { missing_values })
        } else {
            Ok(HanoiTowersDiscs {
                discs,
                depth: max_val,
            })
        }
    }

    pub fn tower_discs(&self, tower: HanoiTower) -> &Vec<usize> {
        &self.discs[tower.index()]
    }
    pub fn all_discs(&self) -> &[Vec<usize>; 3] {
        &self.discs
    }
    pub fn depth(&self) -> usize {
        self.depth
    }
}

impl HanoiDiscMover for HanoiTowersDiscs {
    fn move_disc(&mut self, src: HanoiTower, dest: HanoiTower) {
        if src != dest {
            let value = self.discs[src.index()].pop().expect("Missing disc");
            self.discs[dest.index()].push(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        DiscMoveStore, HanoiDiscMover, HanoiSolver, HanoiTower, HanoiTowersDiscs, InvalidDiscsError,
    };

    #[test]
    fn hanoi_tower_number() {
        assert_eq!(1, HanoiTower::T1.number());
        assert_eq!(2, HanoiTower::T2.number());
        assert_eq!(3, HanoiTower::T3.number());
    }

    #[test]
    fn hanoi_tower_to_string() {
        assert_eq!(String::from("T1"), HanoiTower::T1.to_string());
        assert_eq!(String::from("T2"), HanoiTower::T2.to_string());
        assert_eq!(String::from("T3"), HanoiTower::T3.to_string());
    }

    #[test]
    fn store_disc_moves() {
        let mut store = DiscMoveStore::default();
        store.move_disc(HanoiTower::T1, HanoiTower::T3);
        store.move_disc(HanoiTower::T2, HanoiTower::T1);
        assert_eq!(
            vec![
                (HanoiTower::T1, HanoiTower::T3),
                (HanoiTower::T2, HanoiTower::T1)
            ],
            store.0
        )
    }

    #[test]
    fn solve_hanoi_level0() {
        let mut store = DiscMoveStore::default();
        let mut solver = HanoiSolver::build(&mut store);
        solver.solve(0);
        let no_moves: Vec<(HanoiTower, HanoiTower)> = vec![];
        assert_eq!(no_moves, store.0);
    }

    #[test]
    fn solve_hanoi_level1() {
        let mut store = DiscMoveStore::default();
        let mut solver = HanoiSolver::build(&mut store);
        solver.solve(1);
        assert_eq!(vec![(HanoiTower::T1, HanoiTower::T3)], store.0);
    }

    #[test]
    fn solve_hanoi_level2() {
        let mut store = DiscMoveStore::default();
        let mut solver = HanoiSolver::build(&mut store);
        solver.solve(2);
        let moves = vec![
            (HanoiTower::T1, HanoiTower::T2),
            (HanoiTower::T1, HanoiTower::T3),
            (HanoiTower::T2, HanoiTower::T3),
        ];
        assert_eq!(moves, store.0);
    }

    #[test]
    fn solve_hanoi_level3() {
        let mut store = DiscMoveStore::default();
        let mut solver = HanoiSolver::build(&mut store);
        solver.solve(3);
        let moves = vec![
            (HanoiTower::T1, HanoiTower::T3),
            (HanoiTower::T1, HanoiTower::T2),
            (HanoiTower::T3, HanoiTower::T2),
            (HanoiTower::T1, HanoiTower::T3),
            (HanoiTower::T2, HanoiTower::T1),
            (HanoiTower::T2, HanoiTower::T3),
            (HanoiTower::T1, HanoiTower::T3),
        ];
        assert_eq!(moves, store.0);
    }

    #[test]
    fn move_discs() {
        let mut htd = HanoiTowersDiscs::build(4);
        assert_eq!(&[vec![3, 2, 1, 0], vec![], vec![]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T3);
        assert_eq!(&[vec![3, 2, 1], vec![], vec![0]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T3);
        assert_eq!(&[vec![3, 2], vec![], vec![0, 1]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T2);
        assert_eq!(&[vec![3], vec![2], vec![0, 1]], htd.all_discs());
        htd.move_disc(HanoiTower::T3, HanoiTower::T2);
        assert_eq!(&[vec![3], vec![2, 1], vec![0]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T2);
        assert_eq!(&[vec![], vec![2, 1, 3], vec![0]], htd.all_discs());
    }

    #[test]
    fn depth() {
        let mut htd = HanoiTowersDiscs::build(4);
        assert_eq!(4, htd.depth());
    }

    #[test]
    fn build_from_discs_valid() {
        let discs: Vec<[Vec<usize>; 3]> = vec![
            [vec![3, 2, 1, 0], vec![], vec![]],
            [vec![3, 2, 1], vec![], vec![0]],
            [vec![3, 2], vec![], vec![0, 1]],
            [vec![3], vec![2], vec![0, 1]],
            [vec![3], vec![2, 1], vec![0]],
            [vec![], vec![2, 1, 3], vec![0]],
            [vec![0, 1], vec![2], vec![]],
        ];
        for disc in discs {
            let hanoi_tower =
                HanoiTowersDiscs::build_from_discs(disc.clone()).expect("Missing disc");
            assert_eq!(disc, *(hanoi_tower.all_discs()));
        }
    }

    #[test]
    fn invalid_build_from_discs_panic() {
        let discs: Vec<[Vec<usize>; 3]> = vec![
            [vec![2, 1, 0], vec![], vec![4]],
            [vec![3, 1], vec![], vec![0]],
            [vec![3], vec![], vec![1]],
            [vec![3], vec![2], vec![0]],
            [vec![], vec![3], vec![]],
            [vec![], vec![2], vec![]],
        ];

        let missing_discs: Vec<Vec<usize>> = vec![
            vec![3],
            vec![2],
            vec![0, 2],
            vec![1],
            vec![0, 1, 2],
            vec![0, 1],
        ];
        for (disc, missing_values) in discs.iter().zip(missing_discs) {
            let hanoi_tower = HanoiTowersDiscs::build_from_discs(disc.clone());
            assert_eq!(Err(InvalidDiscsError { missing_values }), hanoi_tower);
        }
    }
}
