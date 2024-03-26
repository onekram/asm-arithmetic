#!/usr/bin/env python3

import random
import sys
from subprocess import Popen, PIPE

sys.set_int_max_str_digits(6000)


def gen_input(stream, down: int, up: int, log: bool) -> int:
    assert down <= up
    num1 = random.randint(1 << (64 * down), 1 << (64 * up))
    num2 = random.randint(1 << (64 * down), 1 << (64 * up))
    if log:
        print(num1, 'Len first: ', len(str(num1)), end='\n\n')
        print(num2, 'Len second:', len(str(num2)), end='\n\n')
    else:
        print(f"Len first: {len(str(num1))}")
        print(f"Len second: {len(str(num2))}")

    stream.write(str(num1).encode('utf-8'))
    stream.write('\n'.encode('utf-8'))
    stream.write(str(num2).encode('utf-8'))
    stream.write('\n'.encode('utf-8'))
    return num1 * num2

    


def test(test_number: int, down: int, up: int, log: int=False) -> None:
    with Popen('./build/mul', shell=True, stdin=PIPE, stdout=PIPE, close_fds=True) as process:
        res = gen_input(process.stdin, down, up, log)

        process.stdin.close()
        if process.wait() != 0:
            print('Return code is not 0!')
            exit(1)

        real = process.stdout.read().decode('utf-8')
        if res != int(real):
            if log:
                print(f'Expected {res}, but find: \n\n\n{int(real)} \nLen: {len(real)} ')
            else:
                print(f"Wrong answer, Len: {len(real)}")
            return
        print(f'Test {test_number} completed successfully. Len res: {len(real)}')

def test_runner(count: int) -> None: 
    for i in range(count):
        test(i + 1, 126, 127)

if __name__ == '__main__':
    test_runner(100)
