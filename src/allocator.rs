use bumpalo::Bump;

pub struct BumpAllocator {
    allocator: Bump,
}
impl BumpAllocator {
    pub fn new() -> Self {
        BumpAllocator {
            allocator: Bump::new(),
        }
    }
}

impl Drop for BumpAllocator {
    fn drop(&mut self) {
        self.allocator.reset();
        println!("Bump reseted");
    }
}

pub trait Allocation<T> {
    fn alloc(&self, item: T) -> &mut T;

    fn dealloc(&self);
}

impl<T> Allocation<T> for BumpAllocator {
    fn alloc(&self, item: T) -> &mut T {
        self.allocator.alloc(item)
    }

    fn dealloc(&self) {
        panic!("This allocator whould never dealloc");
    }
}
