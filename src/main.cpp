#include "includes.h"

#include "melodies/ecstasy-3.h"

player::StcPlayer g_player;

void Timer50Hz()
{
    g_player.Play();
    ay::UpdateEmulatorParameters(g_player.GetAy(), g_player.UpdateEnvelope());
}

int main()
{
    // Setup Timer1 to produce high-speed PWM (~250 KHz) and enable output on OC1B (PB4)
    DDRB = BV(4);
    PLLCSR = BV(PCKE) | BV(PLLE);
    TCCR1 = BV(CS10);
    GTCCR = BV(PWM1B) | BV(COM1B1);
    OCR1B = 0x00;
    OCR1C = 0xFF;

    // Setup Timer0 to generate interrupts each 368 CPU clocks
    TCCR0A = BV(WGM01);
    TCCR0B = BV(CS01);
    OCR0A = 45;
    TIMSK = BV(OCIE0A);
    sei();

    /*
    ay::Ay38910 ay;
    ZeroStructure(ay);
    ay.m_volumeA = 31;
    ay.m_volumeB = 5;
    ay.m_tonePeriodA = 161;
    ay.m_tonePeriodB = 100;
    ay.m_mixer = 0b00111111;
    ay.m_envelopePeriod = 0;
    ay.m_envelopeShape = 0x0C;
    ay::UpdateEmulatorParameters(ay, true);

    for(;;)
    {
    }
    */

    g_player.Init(g_melodyExtasy3);
    
    for(;;)
    {
    }
}
