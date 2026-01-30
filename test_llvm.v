// Comprehensive LLVM backend test

// Test arithmetic operations
test_arithmetic a b : {
  print(a + b);
  print(a - b);
  print(a * b);
  print(a / b);
  print(a % b);
};

// Test comparison operations
test_comparisons x y : {
  print(x == y);
  print(x != y);
  print(x < y);
  print(x > y);
};

// Test conditionals
test_conditionals x : {
  if (x > 40) then print(100);
  if (x < 40) then print(200) else print(300);
};

// Test loop
test_loop n : for (i:0; i < n; i++) print(i);

// Test function with return
add x y : x + y;

// Test floating point
test_float f g : {
  print(f + g);
  print(f * g);
};

// Test nested expressions
test_expr : {
  print((2 + 3) * (4 - 1));
  print(1 + 2 + 3);
};

// Run all tests
print("=== Arithmetic (10, 3) ===");
test_arithmetic 10, 3;

print("=== Comparisons (5, 10) ===");
test_comparisons 5, 10;

print("=== Conditionals (42) ===");
test_conditionals(42);

print("=== Loop (0 to 2) ===");
test_loop(3);

print("=== Function add(7, 8) ===");
result : add 7 8;
print(result);

print("=== Float (3.14, 2.0) ===");
test_float 3.14, 2.0;

print("=== Expressions ===");
test_expr();

print("=== Direct operations ===");
a : 5;
b : 3;
print(a + b);
print(a * b);

x : 10;
if (x > 5) then print(999);

print("=== DONE ===");
