use bumpalo::Bump;
use core::fmt::Debug;

pub struct BumpAllocator {
    allocator: Bump,
}

impl Debug for BumpAllocator {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        fmt.debug_struct("BumpAllocator").finish()
    }
}

#[allow(dead_code)]
impl BumpAllocator {
    pub fn new() -> Self {
        BumpAllocator {
            allocator: Bump::new(),
        }
    }
}

#[allow(dead_code)]
impl Drop for BumpAllocator {
    fn drop(&mut self) {
        self.allocator.reset();
        println!("Bump reseted");
    }
}

pub trait Allocation<T> {
    fn alloc(&mut self, item: T) -> &mut T;

    fn dealloc(&self);
}

#[allow(dead_code)]
impl BumpAllocator {
    pub fn alloc_string(&mut self, s: &str) -> &mut str {
        self.allocator.alloc_str(s)
    }
}

#[allow(dead_code)]
impl<T> Allocation<T> for BumpAllocator {
    fn alloc(&mut self, item: T) -> &mut T {
        self.allocator.alloc(item)
    }

    fn dealloc(&self) {
        panic!("This allocator whould never dealloc");
    }
}
