const std = @import("std");

pub fn main() !void {
    var x: i32 = 0;
    var y: i32 = 100;

    while (x == y) : ({
        x += 1;
        y -= 10;
    }) {
        std.debug.print("{}, {}", .{ x, y });
    }
}
