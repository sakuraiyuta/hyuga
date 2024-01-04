import argparse
import hy
from .server import start
from .version import get_version


def main():
    parser = argparse.ArgumentParser()
    parser.description = 'hyuga - Yet another hy language server'

    parser.add_argument(
        '--version', action='store_true',
        help='Print version and exit'
    )

    args = parser.parse_args()

    if args.version:
        version = get_version()
        print(f'{__package__} - v{version}')
        return

    start()


if __name__ == '__main__':
    main()
