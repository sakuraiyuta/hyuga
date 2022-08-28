import argparse
import hy
from .server import start


def main():
    parser = argparse.ArgumentParser()
    parser.description = 'hyuga - Yet another hy language server'

    parser.add_argument(
        '--version', action='store_true',
        help='Print version and exit'
    )

    args = parser.parse_args()

    if args.version:
        print('hyuga v0.0.1')
        return

    start()


if __name__ == '__main__':
    main()
