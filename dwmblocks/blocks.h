//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
//	{"", "~/.local/bin/dwm_resources.sh",	1,		10},
    {"|",   "~/.local/bin/dwm_battery.sh"                ,60      ,0 },
    {"|",  "~/.local/bin/dwm_backlight.sh"                 ,0     ,10},
    {"|","~/.local/bin/dwm_alsa.sh"                    ,0      ,10},
    {"|📅", "date '+%b %d (%a) %X'",					1,		0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] =" ";
static unsigned int delimLen = 5;
