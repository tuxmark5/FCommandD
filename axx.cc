#include <fcntl.h>
#include <stdio.h>
#include <linux/uinput.h>


int main()
{
  int fd = open("/dev/uinput", O_RDONLY);
  ioctl(fd, UI_SET_EVBIT, EV_SYN);
}

