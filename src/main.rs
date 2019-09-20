mod font;

use std::io::prelude::*;
use std::fs::File;
use rand::Rng;
use minifb::{WindowOptions, Window, Scale, Key};
use font::FONT_MAP;

// Programs are stored from 0x200 to 0xFFF
const MEMORY_SIZE: usize = 4096;
const SCREEN_WIDTH: usize = 64;
const SCREEN_HEIGHT: usize = 32;

struct Sjip8 {
	/// General purpose registers (V0-VF)
	registers: [u8; 16],

	/// I register
	reg_i: usize,

	/// Delay timer
	reg_dt: u8,

	/// Sound timer
	reg_st: u8,

	/// Program Counter
	pc: usize,

	/// Stack Pointer
	sp: usize,
	/// Stack
	stack: [u16; 16],

	/// Memory
	memory: [u8; MEMORY_SIZE],

	/// Video memory
	pub vram: [u8; SCREEN_WIDTH * SCREEN_HEIGHT],

	/// Keys
	pub keys: [bool; 16],

	/// Register to store key press in
	blocking_key_register: Option<usize>,
}

impl Sjip8 {
	fn with_rom(rom: Vec<u8>) -> Self {
		let mut sjip: Sjip8 = Default::default();
		for (addr, &byte) in rom.iter().enumerate() {
			sjip.memory[0x200 + addr] = byte;
		}

		sjip
	}

	fn get_op(&self) -> u16 {
		(self.memory[self.pc] as u16) << 8 | self.memory[(self.pc + 1)] as u16
	}

	fn step(&mut self) {
		// Decrement timers
		self.reg_dt = self.reg_dt.saturating_sub(1);
		self.reg_st = self.reg_st.saturating_sub(1);

		// Check if we're waiting for a key press, and if we are
		// then check if any key has been pressed.
		if let Some(reg) = self.blocking_key_register {
			for (i, &key_is_pressed) in self.keys.iter().enumerate() {
				if key_is_pressed {
					self.registers[reg] = i as u8;
					self.blocking_key_register = None;
				}
			}

			if self.blocking_key_register.is_some() {
				return;
			}
		}

		// Execute operation
		self.execute_op(self.get_op());
	}

	fn execute_op(&mut self, op: u16) {
		let nibbles = (
			(op & 0xf000) >> 12,
			(op & 0x0f00) >> 8,
			(op & 0x00f0) >> 4,
			(op & 0x000f)
		);

		let nnn = op & 0x0fff;
		let x = nibbles.1 as usize;
		let y = nibbles.2 as usize;
		let n = nibbles.3 as usize;
		let kk = (op & 0x00ff) as u8;

		let next_pc = match nibbles {
			(0x00, 0x00, 0x0e, 0x00) => {
				self.vram = [0; SCREEN_WIDTH * SCREEN_HEIGHT];
				self.pc + 2
			},
			(0x00, 0x00, 0x0e, 0x0e) => {
				// Return
				let addr = self.stack[self.sp];
				self.sp = self.sp - 1;

				addr as usize
			},
			(0x00, _, _, _) => self.pc + 2,
			(0x01, _, _, _) => {
				dbg!(nnn);
				// Jump to nnn
				nnn as usize
			},
			(0x02, _, _, _) => {
				// Call nnn
				self.sp = self.sp + 1;
				self.stack[self.sp] = self.pc as u16 + 2;

				nnn as usize
			},
			(0x03, _, _, _) => {
				// Skip if Vx == kk
				if self.registers[x] == kk {
					self.pc + 4
				} else {
					self.pc + 2
				}
			},
			(0x04, _, _, _) => {
				// Skip if Vx != kk
				if self.registers[x] != kk {
					self.pc + 4
				} else {
					self.pc + 2
				}
			},
			(0x05, _, _, 0x00) => {
				// Skip if Vx == Vy
				if self.registers[x] == self.registers[y] {
					self.pc + 4
				} else {
					self.pc + 2
				}
			},
			(0x06, _, _, _) => {
				// Set Vx = kk
				self.registers[x] = kk;

				self.pc + 2
			},
			(0x07, _, _, _) => {
				// Set Vx = Vx + kk
				self.registers[x] = self.registers[x].wrapping_add(kk);

				self.pc + 2
			},
			(0x08, _, _, 0x00) => {
				// Set Vx = Vy
				self.registers[x] = self.registers[y];

				self.pc + 2
			},
			(0x08, _, _, 0x01) => {
				// Set Vx = Vx | Vy
				self.registers[x] = self.registers[x] | self.registers[y];

				self.pc + 2
			},
			(0x08, _, _, 0x02) => {
				// Set Vx = Vx & Vy
				self.registers[x] = self.registers[x] & self.registers[y];

				self.pc + 2
			},
			(0x08, _, _, 0x03) => {
				// Set Vx = Vx ^ Vy
				self.registers[x] = self.registers[x] ^ self.registers[y];

				self.pc + 2
			},
			(0x08, _, _, 0x04) => {
				// Set Vx = Vx + Vy, VF = carry
				let (new_value, overflowed) = self.registers[x].overflowing_add(self.registers[y]);
				self.registers[x] = new_value;

				self.registers[15] = if overflowed {
					1
				} else {
					0
				};

				self.pc + 2
			},
			(0x08, _, _, 0x05) => {
				// Set Vx = Vx - Vy, VF = NOT borrow
				let vx = self.registers[x];
				let vy = self.registers[y];

				self.registers[15] = if vx > vy {
					1
				} else {
					0
				};
				self.registers[x] = vx.wrapping_sub(vy);

				self.pc + 2
			},
			(0x08, _, _, 0x06) => {
				// Set Vx = Vx >> 1
				self.registers[15] = self.registers[x] & 1;
				self.registers[x] = self.registers[x] >> 1;
				self.pc + 2
			},
			(0x08, _, _, 0x07) => {
				// Set Vx = Vy - Vx, VF = NOT borrow
				let vx = self.registers[x];
				let vy = self.registers[y];

				self.registers[15] = if vx < vy {
					1
				} else {
					0
				};
				self.registers[x] = vy.wrapping_sub(vx);

				self.pc + 2
			},
			(0x08, _, _, 0x0e) => {
				// Set Vx = Vx << 1
				self.registers[15] = (self.registers[x] & 0b10000000) >> 7;
				self.registers[x] = self.registers[x] << 1;
				self.pc + 2
			},
			(0x09, _, _, 0x00) => {
				// Skip if Vx != Vy
				if self.registers[x] != self.registers[y] {
					self.pc + 4
				} else {
					self.pc + 2
				}
			},
			(0x0a, _, _, _) => {
				// Set I = nnn
				self.reg_i = nnn as usize;

				self.pc + 2
			},
			(0x0b, _, _, _) => {
				// Jump to nnn + V0
				(nnn + self.registers[0] as u16) as usize
			},
			(0x0c, _, _, _) => {
				// Set Vx = random byte & kk + V0
				let byte = rand::thread_rng().gen::<u8>();
				self.registers[x] = (byte & kk).wrapping_add(self.registers[0]);

				self.pc + 2
			},
			(0x0d, _, _, _) => {
				// Display sprite of size n from memory starting at I
				// at screen coordinates (Vx, Vy)
				let sprite_x = self.registers[x] as usize;
				let sprite_y = self.registers[y] as usize;

				self.registers[15] = 0;
				for i in 0..n {
					let screen_y = (sprite_y + i) % SCREEN_HEIGHT;
					for j in 0..8 {
						let screen_x = (sprite_x + j) % SCREEN_WIDTH;
						let color = (self.memory[self.reg_i + (i as usize)] >> (7 - j)) & 1;
						self.registers[15] |= color & self.vram[screen_x + screen_y * SCREEN_WIDTH];
						self.vram[screen_x + screen_y * SCREEN_WIDTH] ^= color;
					}
				}

				self.pc + 2
			},
			(0x0e, _, 0x09, 0x0e) => {
				// Skip if button in Vx is pressed
				if self.keys[self.registers[x] as usize] {
					self.pc + 4
				} else {
					self.pc + 2
				}
			},
			(0x0e, _, 0x0a, 0x01) => {
				// Skip if button in Vx is not pressed
				if !self.keys[self.registers[x] as usize] {
					self.pc + 4
				} else {
					self.pc + 2
				}
			},
			(0x0f, _, 0x00, 0x07) => {
				// Set Vx = DT
				self.registers[x] = self.reg_dt;

				self.pc + 2
			},
			(0x0f, _, 0x00, 0x0a) => {
				self.blocking_key_register = Some(x);
				self.pc + 2
			},
			(0x0f, _, 0x01, 0x05) => {
				// Set DT = Vx
				self.reg_dt = self.registers[x];

				self.pc + 2
			},
			(0x0f, _, 0x01, 0x08) => {
				// Set ST = Vx
				self.reg_st = self.registers[x];

				self.pc + 2
			},
			(0x0f, _, 0x01, 0x0e) => {
				// Set I = I + Vx
				self.reg_i = self.reg_i + self.registers[x] as usize;

				self.pc + 2
			},
			(0x0f, _, 0x02, 0x09) => {
				self.reg_i = (self.registers[x]) as usize * 5;
				self.pc + 2
			},
			(0x0f, _, 0x03, 0x03) => {
				// BCD stuff
				self.memory[self.reg_i] = self.registers[x] / 100;
				self.memory[self.reg_i + 1] = (self.registers[x] % 100) / 10;
				self.memory[self.reg_i + 2] = self.registers[x] % 10;
				self.pc + 2
			},
			(0x0f, _, 0x05, 0x05) => {
				// Store registers V0 through Vx in memory starting at I
				for reg in 0..=x {
					self.memory[self.reg_i + reg] = self.registers[reg];
				}
				self.pc + 2
			},
			(0x0f, _, 0x06, 0x05) => {
				// Read registers V0 through Vx from memory starting at I
				for reg in 0..=x {
					self.registers[reg] = self.memory[self.reg_i + reg];
				}
				self.pc + 2
			},
			_ => panic!("Unimplemented: {:#06X?}", op)
		};

		self.pc = next_pc;
	}
}

impl Default for Sjip8 {
	fn default() -> Self {
		let mut sjip = Sjip8 {
			registers: [0; 16],
			reg_i: 0,
			reg_dt: 0,
			reg_st: 0,
			pc: 0x200,
			sp: 0,
			stack: [0; 16],
			memory: [0; MEMORY_SIZE],
			vram: [0; SCREEN_WIDTH * SCREEN_HEIGHT],
			keys: [false; 16],
			blocking_key_register: None,
		};

		for (addr, &word) in FONT_MAP.iter().enumerate() {
			sjip.memory[addr] = word;
		}

		sjip
	}
}

fn main() {
	// Load ROM
	let open_file_result = nfd::open_file_dialog(None, None).unwrap();
	let mut emu = match open_file_result {
		nfd::Response::Okay(path) => {
			let mut file = File::open(path).unwrap();
			let mut file_buffer = Vec::new();

			file.read_to_end(&mut file_buffer).unwrap();

			Sjip8::with_rom(file_buffer)
		},
		_ => panic!("No ROM selected")
	};

	// Setup screen
	const keys: [Key; 16] = [
		Key::X, Key::Key1, Key::Key2, Key::Key3,
		Key::Q, Key::W, Key::E, Key::A,
		Key::S, Key::D, Key::Z, Key::C,
		Key::Key4, Key::R, Key::F, Key::V,
	];
	let mut buffer = vec![0u32; SCREEN_WIDTH * SCREEN_HEIGHT];
  let mut window = Window::new(
  	"SJIP8",
  	SCREEN_WIDTH,
  	SCREEN_HEIGHT,
  	WindowOptions {
    scale: Scale::X8,
    ..WindowOptions::default()
  	}
  ).unwrap();

	loop {
		// Step emulator
		emu.step();

		// Render
		for (i, &pixel) in emu.vram.iter().enumerate() {
			if pixel > 0 {
				buffer[i] = 0xaa00ff;
			} else {
				buffer[i] = 0x330077;
			}
		}
		window.update_with_buffer(&buffer).unwrap();

		// Capture keys
		for (i, &key) in keys.iter().enumerate() {
			emu.keys[i] = window.is_key_down(key);
		}

		// Wait a bit
		::std::thread::sleep(::std::time::Duration::from_millis(5));
	}
}
