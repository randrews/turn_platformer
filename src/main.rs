use std::fmt::{Display, Formatter, Write};
use std::io::{stdin, stdout};
use std::ops::{Add, Index};
use cgmath::Vector2;
use termion::color::{Bg, Black, Blue, Fg, Red, Reset, White, Yellow};
use termion::event::{Event, Key};
use termion::input::TermReadEventsAndRaw;
use termion::raw::IntoRawMode;
use crate::MapCell::OffMap;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Maneuver {
    WalkLeft,
    WalkRight,
    JumpLeft,
    JumpRight,
    JumpUp,
    ClimbUp,
    ClimbDown,
    Fall(Momentum),
    Wait
}

impl Maneuver {
    pub fn key(&self) -> char {
        match self {
            Maneuver::WalkLeft => 'a',
            Maneuver::WalkRight => 'd',
            Maneuver::JumpLeft => 'q',
            Maneuver::JumpRight => 'e',
            Maneuver::JumpUp => 'w',
            Maneuver::ClimbUp => 'w',
            Maneuver::ClimbDown => 'x',
            Maneuver::Wait => 's',
            Maneuver::Fall(_) => 's'
        }
    }

    pub fn delta(&self) -> Vector2<isize> {
        match self {
            Maneuver::WalkLeft => Vector2::new(-1, 0),
            Maneuver::WalkRight => Vector2::new(1, 0),
            Maneuver::JumpLeft => Vector2::new(-1, -1),
            Maneuver::JumpRight => Vector2::new(1, -1),
            Maneuver::JumpUp => Vector2::new(0, -1),
            Maneuver::ClimbUp => Vector2::new(0, -1),
            Maneuver::ClimbDown => Vector2::new(0, 1),
            Maneuver::Wait => Vector2::new(0, 0),
            Maneuver::Fall(Momentum::None) => Vector2::new(0, 1),
            Maneuver::Fall(Momentum::Left) => Vector2::new(-1, 1),
            Maneuver::Fall(Momentum::Right) => Vector2::new(1, 1),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Momentum {
    Left,
    Right,
    None
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum MapCell {
    Empty,
    Wall,
    Ladder,
    OffMap
}

impl Display for MapCell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MapCell::Empty => f.write_char(' '),
            MapCell::Wall => write!(f, "{}#", Fg(Red)),
            MapCell::Ladder => write!(f, "{}|", Fg(White)),
            MapCell::OffMap => write!(f, "{}{}.", Fg(Black), Bg(Red)),
        }
    }
}

const LEVEL: [&str; 8] = [
    "################",
    "#..............#",
    "#........|.....#",
    "#........|.....#",
    "#......##.#.#..#",
    "#.....#........#",
    "#.@..|.........#",
    "################"
];

struct GameState {
    player_loc: MapCoord,
    map_size: Vector2<usize>,
    level: Vec<MapCell>,
    momentum: Momentum
}

impl GameState {
    fn from_strings<T: IntoIterator<Item: AsRef<str>>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let level_strings = Vec::from_iter(iter);
        let height = level_strings.len();
        let width = level_strings[0].as_ref().len();
        let mut player_loc = None;
        let mut level = Vec::with_capacity(height * width);
        for (y, line) in level_strings.into_iter().enumerate() {
            for (x, ch) in line.as_ref().chars().enumerate() {
                let cell = match ch {
                    '#' => MapCell::Wall,
                    '|' => MapCell::Ladder,
                    '@' => {
                        player_loc = Some((x, y).into());
                        MapCell::Empty
                    }
                    _ => MapCell::Empty
                };
                level.push(cell);
            }
        }
        Self {
            player_loc: player_loc.unwrap(),
            map_size: (width, height).into(),
            level,
            momentum: Momentum::None
        }
    }

    fn size(&self) -> Vector2<usize> {
        self.map_size
    }

    fn supported(&self) -> bool {
        let support = self[self.player_loc + (0, 1)];
        (support != MapCell::Empty && support != OffMap) ||
            self[self.player_loc] == MapCell::Ladder
    }

    fn navigable<T: Into<MapCoord>>(&self, coord: T) -> bool {
        let c = self[coord.into()];
        c == MapCell::Empty || c == MapCell::Ladder
    }

    fn all_moves(&self) -> Vec<Maneuver> {
        let mut moves = vec![];
        let supported = self.supported();
        let wall_left = self[self.player_loc + (-1, 0)] == MapCell::Wall;
        let wall_right = self[self.player_loc + (1, 0)] == MapCell::Wall;
        let on_ladder = self[self.player_loc] == MapCell::Ladder;
        let above_ladder = self[self.player_loc + (0, 1)] == MapCell::Ladder;
        let fall_target = self.player_loc + Maneuver::Fall(self.momentum).delta();

        if supported { // On the ground or a ladder, we can walk or jump or just stand there
            moves.push(Maneuver::WalkLeft);
            moves.push(Maneuver::WalkRight);
            moves.push(Maneuver::JumpUp);
            moves.push(Maneuver::Wait);
        } else { // Unsupported, gravity takes us
            // If the place our momentum takes us is navigable, use that
            if self.navigable(fall_target) {
                moves.push(Maneuver::Fall(self.momentum))
            } else { // Otherwise, kill the momentum
                moves.push(Maneuver::Fall(Momentum::None))
            }
        }

        // If we're on solid ground or next to a wall, and the target is clear, we can jump
        if supported || wall_right { moves.push(Maneuver::JumpLeft) }
        if supported || wall_left { moves.push(Maneuver::JumpRight) }

        // If we're on a ladder, we can climb up
        if on_ladder { moves.push(Maneuver::ClimbUp) }

        // On or above a ladder, climb down
        if on_ladder || above_ladder { moves.push(Maneuver::ClimbDown) }

        // Only give the moves that don't lead us into a wall or off-map
        moves.into_iter()
            .filter(|m| self.navigable(self.player_loc + m.delta()))
            .collect()
    }

    fn make_move(&mut self, maneuver: Maneuver) {
        self.player_loc = self.player_loc + maneuver.delta();
        self.momentum = match maneuver {
            Maneuver::JumpLeft => Momentum::Left,
            Maneuver::JumpRight => Momentum::Right,
            _ => Momentum::None
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct MapCoord(Vector2<isize>);

macro_rules! mapcoord_from_numeric {
    ($numeric:ty) => {
        impl From<Vector2<$numeric>> for MapCoord {
            fn from(coord: Vector2<$numeric>) -> Self { Self(Vector2 { x: coord.x as isize, y: coord.y as isize }) }
        }

        impl From<($numeric, $numeric)> for MapCoord {
            fn from(c: ($numeric, $numeric)) -> Self { Self(Vector2::new(c.0 as isize, c.1 as isize)) }
        }
    };
}

mapcoord_from_numeric!(usize);
mapcoord_from_numeric!(isize);
mapcoord_from_numeric!(u32);
mapcoord_from_numeric!(i32);

impl<T: Into<MapCoord>> Add<T> for MapCoord {
    type Output = MapCoord;
    fn add(self, rhs: T) -> Self::Output {
        let rhs = rhs.into();
        Self(self.0 + rhs.0)
    }
}

impl<T: Into<MapCoord>> Index<T> for GameState {
    type Output = MapCell;
    fn index(&self, index: T) -> &Self::Output {
        let MapCoord(index) = index.into();
        if index.x < 0 || index.y < 0 || index.x >= self.map_size.x as isize || index.y >= self.map_size.y as isize {
            &OffMap
        } else {
            &self.level[index.x as usize + index.y as usize * self.map_size.x]
        }
    }
}

impl Display for GameState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let size = self.size();
        let possible_moves = self.all_moves();
        let move_tuples = possible_moves.into_iter().map(|m| {
            let ch = m.key();
            let target = self.player_loc + m.delta();
            (target, ch, m)
        }).collect::<Vec<_>>();

        for y in 0..size.y {
            for x in 0..size.x {
                if self.player_loc == (x, y).into() {
                    write!(f, "{}@", Fg(Blue))?;
                } else if let Some((_, ch, _)) = move_tuples.iter().find(|t| t.0 == (x, y).into()) {
                    write!(f, "{}{}", Fg(Yellow), ch)?;
                } else {
                    write!(f, "{}", self[(x, y)])?;
                }
            }
            write!(f, "\r\n")?;
        }
        write!(f, "{}{}", Fg(Reset), Bg(Reset))?;
        Ok(())
    }
}

fn move_for_key(key: char, game_state: &GameState) -> Option<Maneuver> {
    let all = game_state.all_moves();
    all.into_iter().find(|m| m.key() == key)
}

fn main() {
    let mut game = GameState::from_strings(LEVEL);
    {
        // Make a RawTerminal for its side effects
        let rt = stdout().into_raw_mode().unwrap();
        let _ = rt;

        let mut exit = false;
        print!("{}", game);
        for e in stdin().events_and_raw() {
            if let Ok((Event::Key(Key::Ctrl('c')), _)) = e {
                exit = true;
            } else if let Ok((Event::Key(Key::Char(key)), _)) = e
                && let Some(maneuver) = move_for_key(key, &game) {
                    game.make_move(maneuver);
            }
            if exit { break }
            else { print!("{}", game) }
        }
    }
    println!("Bye!")
}
