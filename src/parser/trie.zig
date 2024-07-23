const std = @import("std");

const Allocator = std.mem.Allocator;
const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;

pub fn TrieNode(T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        children: [26]?*Self,
        terminal: ?T,

        fn create(allocator: Allocator) error{OutOfMemory}!*Self {
            const trie_node = try allocator.create(Self);

            trie_node.allocator = allocator;
            trie_node.children = [_]?*Self{null} ** 26;
            trie_node.terminal = null;

            return trie_node;
        }

        fn deinit(self: *Self) void {
            for (self.children) |child_opt| {
                if (child_opt) |child| {
                    child.deinit();
                }
            }

            self.allocator.destroy(self);
        }
    };
}

pub fn Trie(T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        root: *TrieNode(T),

        pub fn init(allocator: Allocator) error{OutOfMemory}!Self {
            return .{
                .allocator = allocator,
                .root = try TrieNode(T).create(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.root.deinit();
        }

        pub fn insert(
            self: *Self,
            word: []const u8,
            value: T,
        ) error{OutOfMemory}!void {
            var current_node = self.root;

            for (word) |char| {
                const index = char - 'a';

                if (current_node.children[index]) |child| {
                    current_node = child;
                } else {
                    current_node.children[index] = try TrieNode(T).create(self.allocator);
                    current_node = current_node.children[index].?;
                }
            }

            current_node.terminal = value;
        }

        pub fn search(self: *Self, word: []const u8) error{NotFound}!T {
            var current_node = self.root;

            for (word) |char| {
                const index = char - 'a';

                if (current_node.children[index]) |child| {
                    current_node = child;
                } else {
                    return error.NotFound;
                }
            }

            return if (current_node.terminal) |value| value else error.NotFound;
        }
    };
}

test {
    var trie = try Trie(u8).init(std.testing.allocator);
    defer trie.deinit();

    try expectError(error.NotFound, trie.search("none"));
    try trie.insert("none", 0);
    try expectEqual(try trie.search("none"), 0);

    const words = [_][]const u8{ "hello", "hero", "tea", "ted", "ten" };

    for (words, 1..) |word, value| {
        try trie.insert(word, @intCast(value));
    }

    for (words, 1..) |word, value| {
        try expectEqual(try trie.search(word), value);
    }

    const bad_words = [_][]const u8{ "h", "he", "hel", "hell", "hellp", "her", "here", "t", "te", "tek" };

    for (bad_words) |word| {
        try expectError(error.NotFound, trie.search(word));
    }
}
