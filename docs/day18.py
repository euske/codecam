#!/usr/bin/env python
import sys
import fileinput

class Machine:

    def __init__(self):
        self.regs = {}
        self.insts = []
        self.lastsound = None
        return

    def load(self, fp):
        for line in fp:
            args = line.strip().split(' ')
            if not args: continue
            self.insts.append(tuple(args))
            print('inst', args)
        return

    def getvalue(self, v):
        try:
            return int(v)
        except ValueError:
            return self.regs.get(v, 0)

    def setvalue(self, reg, v):
        self.regs[reg] = v
        return

    def exec_inst(self, pc):
        inst = self.insts[pc]
        print('exec', pc, inst, self.regs)
        if inst[0] == 'set':
            y = self.getvalue(inst[2])
            self.setvalue(inst[1], y)
            pc += 1
        elif inst[0] == 'add':
            x = self.getvalue(inst[1])
            y = self.getvalue(inst[2])
            self.setvalue(inst[1], x+y)
            pc += 1
        elif inst[0] == 'mul':
            x = self.getvalue(inst[1])
            y = self.getvalue(inst[2])
            self.setvalue(inst[1], x*y)
            pc += 1
        elif inst[0] == 'mod':
            x = self.getvalue(inst[1])
            y = self.getvalue(inst[2])
            self.setvalue(inst[1], x % y)
            pc += 1
        elif inst[0] == 'jgz':
            v = self.getvalue(inst[1])
            y = self.getvalue(inst[2])
            if 0 < v:
                pc += y
            else:
                pc += 1
        elif inst[0] == 'snd':
            v = self.getvalue(inst[1])
            print ('SOUND', v)
            self.lastsound = v
            pc += 1
        elif inst[0] == 'rcv':
            v = self.getvalue(inst[1])
            if v != 0:
                print('RECOVER', self.lastsound)
                return None
            pc += 1
        else:
            raise ValueError('Invalid instruction: %r' % inst)
        return pc

    def run(self):
        pc = 0
        while pc < len(self.insts):
            pc = self.exec_inst(pc)
            if pc is None: break
        return

m = Machine()
m.load(fileinput.input())
m.run()
