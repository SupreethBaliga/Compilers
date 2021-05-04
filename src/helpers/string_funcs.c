char* strlwr(char* s) {
    char *t = s;
    if (!s) {
        return 0;
    }
    int i = 0;
    while (*t != '\0') {
        if (*t >= 'A' && *t <= 'Z') {
                *t = *t + ('a' - 'A');
        }
        t++;
    }
    return s;
}

char* strupr(char * s) {
    char *t = s;
    if (!s) {
        return 0;
    }
    while(!t != '\0') {
        if(*t >= 'a' && *t <= 'z') {
            *t = *t - ('a' - 'A');
        }
        t++;
    }
    return s;
}

void strcpy2(char *dest, char *source) 
{
    int i = 0;
    while ((dest[i] = source[i]) != '\0')
    {
        i++;
    } 
    return;
}

char* strcat(char *dest, char *source) {
    char *ptr = dest + strlen(dest);
    while(*source != '\0') {
        *ptr = *source;
        ptr ++;
        source++;
    }
    *ptr = '\0';
    return dest;
}

int strcmp(char *s1, char *s2) {
    while(*s1 != '\0') {
        if (*s1 == *s2) {
            s1 ++;
            s2 ++;
        }
        else {
            return *s1 - *s2;
        }
    }
    return 0;
}
