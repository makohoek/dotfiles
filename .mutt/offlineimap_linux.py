#! /usr/bin/python
import subprocess

def get_mail_password():
    out = subprocess.check_output(['pass', 'mail'])
    return out.strip()

if __name__ == '__main__':
    print get_mail_password()
