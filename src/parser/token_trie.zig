const tokenizer_mod = @import("tokenizer.zig");

const Token = tokenizer_mod.Token;

const TokenTrieNode = struct {
    const T = Token.Kind;

    children: [26]?usize = [_]?usize{null} ** 26,
    terminal: ?T = null,
};

fn TokenTrie(comptime memory_size: usize) type {
    return struct {
        const Self = @This();
        const T = Token.Kind;

        memory: [memory_size]TokenTrieNode = [_]TokenTrieNode{.{}} ** memory_size,
        next_index: usize = 0,

        fn addWord(self: *Self, word: []const u8, value: T) void {
            var current_node = &self.memory[0];

            for (word) |char| {
                const index = char - 'a';

                if (current_node.children[index] == null) {
                    current_node.children[index] = self.nextNode();
                }

                current_node = &self.memory[current_node.children[index].?];
            }

            current_node.terminal = value;
        }

        pub fn findWord(self: *const Self, word: []const u8) !T {
            var current_node = &self.memory[0];

            for (word) |char| {
                if (char < 'a' or char > 'z') {
                    return error.NotFound;
                }

                if (current_node.children[char - 'a']) |index| {
                    current_node = &self.memory[index];
                } else {
                    return error.NotFound;
                }
            }

            return if (current_node.terminal) |value| value else error.NotFound;
        }

        fn nextNode(self: *Self) usize {
            self.next_index += 1;

            return self.next_index - 1;
        }
    };
}

pub fn generateTrie(comptime trie_struct: anytype) TokenTrie(calculateMaxTrieSize(trie_struct)) {
    var trie = TokenTrie(calculateMaxTrieSize(trie_struct)){};

    const type_info = @typeInfo(@TypeOf(trie_struct));

    for (0..type_info.Struct.fields.len) |i| {
        const field = trie_struct[i];

        trie.addWord(field[0], @as(Token.Kind, field[1]));
    }

    return trie;
}

fn calculateMaxTrieSize(comptime trie_struct: anytype) usize {
    validateTrieStruct(trie_struct);

    const type_info = @typeInfo(@TypeOf(trie_struct));
    var char_count = 0;

    for (0..type_info.Struct.fields.len) |i| {
        for (trie_struct[i][0]) |_| {
            char_count += 1;
        }
    }

    return char_count;
}

fn validateTrieStruct(comptime trie_struct: anytype) void {
    const Type = @TypeOf(trie_struct);
    const type_info = @typeInfo(Type);

    if (type_info != .Struct or !type_info.Struct.is_tuple) {
        @compileError("argument isn't a tuple struct");
    }

    for (type_info.Struct.fields, 0..) |field, i| {
        const FieldType = field.type;
        const field_type_info = @typeInfo(FieldType);

        if (field_type_info != .Struct or !field_type_info.Struct.is_tuple) {
            @compileError("elements of trie struct should be tuple structs");
        }

        const word_type = @typeInfo(field_type_info.Struct.fields[0].type);
        const type_error = "first element of entry tuple struct should be a pointer to u8 array";

        if (word_type != .Pointer) {
            @compileError(type_error);
        }

        const pointer_type = @typeInfo(word_type.Pointer.child);

        if (pointer_type != .Array) {
            @compileError(type_error);
        }

        const array_type = @typeInfo(pointer_type.Array.child);

        for (trie_struct[i][0]) |char| {
            if (char < 'a' or char > 'z') {
                @compileError("word should consist of lowercase alpha characters");
            }
        }

        if (array_type != .Int or array_type.Int.signedness != .unsigned or array_type.Int.bits != 8) {
            @compileError(type_error);
        }

        const value_type = @typeInfo(field_type_info.Struct.fields[1].type);

        if (value_type != .EnumLiteral) {
            @compileError("second element of entry tuple struct should be an enum literal");
        }
    }
}
