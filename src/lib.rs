use std::io::{BufWriter, Write};

/// Describes the 3 kinds of Hanoï Towers.
#[derive(Debug, PartialEq, Clone)]
pub enum HanoiTower {
    T1,
    T2,
    T3,
}

impl HanoiTower {
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
}

impl ToString for HanoiTower {
    fn to_string(&self) -> String {
        match self {
            HanoiTower::T1 => String::from("T1"),
            HanoiTower::T2 => String::from("T2"),
            HanoiTower::T3 => String::from("T3"),
        }
    }
}

/// Type that can move Hanoï Towers.
/// In practice it's a type that can receive the tower moves.
pub trait HanoiDiscMover {
    /// Move disc from src HanoiTower to dest HanoiTower.
    fn move_disc(&mut self, src: HanoiTower, dest: HanoiTower);
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

    pub fn build(depth: usize, writer: &'a mut T) -> HanoiStepWriter<'a, T> {
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
    discs: [Vec<usize>; 3],
    depth: usize,
}

impl HanoiTowersDiscs {
    pub fn build(depth: usize) -> HanoiTowersDiscs {
        HanoiTowersDiscs {
            discs: [
                (1..=depth).rev().collect(),
                Vec::with_capacity(depth),
                Vec::with_capacity(depth),
            ],
            depth,
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
    use crate::{DiscMoveStore, HanoiDiscMover, HanoiSolver, HanoiTower, HanoiTowersDiscs};

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
        assert_eq!(&[vec![4, 3, 2, 1], vec![], vec![]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T3);
        assert_eq!(&[vec![4, 3, 2], vec![], vec![1]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T3);
        assert_eq!(&[vec![4, 3], vec![], vec![1, 2]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T2);
        assert_eq!(&[vec![4], vec![3], vec![1, 2]], htd.all_discs());
        htd.move_disc(HanoiTower::T3, HanoiTower::T2);
        assert_eq!(&[vec![4], vec![3, 2], vec![1]], htd.all_discs());
        htd.move_disc(HanoiTower::T1, HanoiTower::T2);
        assert_eq!(&[vec![], vec![3, 2, 4], vec![1]], htd.all_discs());
    }

    #[test]
    fn depth() {
        let mut htd = HanoiTowersDiscs::build(4);
        assert_eq!(4, htd.depth());
    }
}
