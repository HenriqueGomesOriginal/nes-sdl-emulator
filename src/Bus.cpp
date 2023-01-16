#include "header/Bus.h"

Bus::Bus()
{
    // Connect CPU to communication bus
    cpu.ConnectBus(this);

    // Clear RAM
    for (auto &i : ram) i = 0x00;
}

Bus::~Bus()
{

}

Bus::cpu()
{

}

// Read and Write

void Bus::write(uint_16 addr, uint8_t data)
{
    if (addr >= 0x0000 && addr <= 0xFFFF)
        ram[addr] = data;
} 

uint_8 Bus::read(uint_16 addr, bool bReadOnly = false)
{
    if (addr >= 0x0000 && addr <= 0xFFFF)
        return ram[addr];
}  