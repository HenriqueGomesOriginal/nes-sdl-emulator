#pragma once

#include <cstdint>
#include <array>

#include "../settings/Compiler.h"
#include "Cpu.h"

class Bus
{
    public:
        Bus();
        ~Bus();

    public: // Devices
        Cpu cpu();

        // RAM
        std::array<uint8_t, 2 * 1024> ram;

    public: // Bus read and write
        void write(uint_16 addr, uint8_t data); // 16 bits of address and 8 bits of data
        uint_8 read(uint_16 addr, bool bReadOnly = false);  
};