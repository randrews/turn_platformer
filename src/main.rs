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
    Fall,
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
            Maneuver::Fall => 's'
        }
    }

    pub fn delta(&self, momentum: Momentum) -> Vector2<isize> {
        match self {
            Maneuver::WalkLeft => Vector2::new(-1, 0),
            Maneuver::WalkRight => Vector2::new(1, 0),
            Maneuver::JumpLeft => Vector2::new(-1, -1),
            Maneuver::JumpRight => Vector2::new(1, -1),
            Maneuver::JumpUp => Vector2::new(0, -1),
            Maneuver::ClimbUp => Vector2::new(0, -1),
            Maneuver::ClimbDown => Vector2::new(0, 1),
            Maneuver::Wait => Vector2::new(0, 0),
            Maneuver::Fall => {
                match momentum {
                    Momentum::Left => Vector2::new(-1, 1),
                    Momentum::Right => Vector2::new(1, 1),
                    Momentum::None => Vector2::new(0, 1)
                }
            },
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
    "#..............#",
    "#..............#",
    "#......##.#....#",
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
        support != MapCell::Empty && support != OffMap
    }

    fn walkable<T: Into<MapCoord>>(&self, coord: T) -> bool {
        let c = self[coord.into()];
        c == MapCell::Empty || c == MapCell::Ladder
    }

    fn all_moves(&self) -> Vec<Maneuver> {
        if !self.supported() {
            vec![Maneuver::Fall]
        } else {
            let mut moves = vec![];
            let all = vec![
                Maneuver::WalkLeft,
                Maneuver::WalkRight,
                Maneuver::JumpLeft,
                Maneuver::JumpRight,
                Maneuver::JumpUp,
                Maneuver::ClimbUp,
                Maneuver::ClimbDown,
                Maneuver::Wait];

            for maneuver in all {
                if self.walkable(self.player_loc + maneuver.delta(self.momentum)) {
                    moves.push(maneuver);
                }
            }
            moves
        }
    }

    fn make_move(&mut self, maneuver: Maneuver) {
        self.player_loc = self.player_loc + maneuver.delta(self.momentum);
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
            let target = self.player_loc + m.delta(self.momentum);
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
        let rt = stdout().into_raw_mode().unwrap();
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
