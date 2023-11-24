import React, {
  ContextType,
  createContext,
  forwardRef,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import { Property } from 'csstype';
import 'material-icons/iconfont/filled.css';
import type { MaterialIconName } from 'ts-material-icon-name-list';

const ThemeContext = createContext({
  bg: '#3d3846' satisfies Property.Color,
  fg: '#9dcd41' satisfies Property.Color,
  blur: 0.6,
  distance: 0.3,
  intensity: 0.4,
  space: 1,
  roundness: 0.5,
});

const useTheme = () => useContext(ThemeContext);

type ThemeType = ContextType<typeof ThemeContext>;

const Theme = ({ children, ...props }: React.PropsWithChildren<Partial<ThemeType>>) => (
  <ThemeContext.Provider value={{ ...useTheme(), ...props }}>{children}</ThemeContext.Provider>
);

const transformControlName = (name: string) => /[AB] \w+: (.+)/.exec(name)?.at(1) ?? name;

type State = {
  pads: Button[];
  knobs: Control[];
  mode: string;
  browser: { isOpen: boolean } & Record<(typeof browserColumn)[number], BrowserResult[]>;
  tracks: Track[];
  loops: Loop[];
};
const browserColumn = ['results', 'tags', 'categories', 'deviceTypes', 'creators'] as const;
type TrackType = 'Group' | 'Instrument' | 'Audio' | 'Hybrid' | 'Effect' | 'Master';
type Track = {
  name: string;
  arm: boolean;
  mute: boolean;
  isChild: boolean;
  isSelected: boolean;
  type: TrackType;
  parentType: TrackType;
  devices: Device[];
};
type Device = {
  deviceName: string;
  presetName: string;
  enabled: boolean;
  isSelected: boolean;
};
type BrowserResult = {
  name: string;
  isSelected: boolean;
};
type Control = {
  name: string;
  bindings: Binding[];
};
type Button = Control & {
  type: 'Button';
  isPressed: boolean;
};
type Knob = Control & {
  type: 'Knob';
};
type Binding = {
  name: string;
  value: number;
  displayedValue: string;
};
type Loop = {
  name: string;
  hasClip: boolean;
  playing: boolean;
  playingQueued: boolean;
  recording: boolean;
  recordingQueued: boolean;
};

const AppStateContext = createContext<State | null>(null);
const useAppState = () => useContext(AppStateContext);

const usePluginConnection = () => {
  const [status, setStatus] = useState<'CLOSED' | 'OPEN' | 'RECEIVING' | 'ERROR'>('CLOSED');
  const [state, setState] = useState<State | null>(null);
  useEffect(() => {
    const socket = new WebSocket(
      `ws://${isDev() ? `${window.location.hostname}:8080` : window.location.host}/ws`
    );
    socket.onopen = (ev) => {
      setStatus('OPEN');
    };
    socket.onmessage = (ev) => {
      setStatus('RECEIVING');
      try {
        setState(JSON.parse(ev.data));
      } catch (e: unknown) {
        setStatus('ERROR');
        socket.close();
      }
    };
    socket.onerror = (ev) => {
      setStatus('ERROR');
      setState(null);
    };
    socket.onclose = (ev) => {
      setStatus('CLOSED');
      setState(null);
    };
    return () => socket.close();
  }, []);

  return { status, state };
};

export const App = () => {
  const rootRef = useRef<HTMLDivElement>(null);
  const { state } = usePluginConnection();
  const [theme, setTheme] = useState(useTheme());
  const updateTheme: UIConfigProps['updateTheme'] = (k) => ({
    value: theme[k],
    onChange: (e: React.ChangeEvent<HTMLInputElement>) =>
      setTheme((theme) => ({
        ...theme,
        [k]: typeof theme[k] === 'number' ? parseFloat(e.target.value) : e.target.value,
      })),
  });

  return (
    <Theme {...theme}>
      <AppStateContext.Provider value={state}>
        <div
          ref={rootRef}
          style={{
            height: '100vh',
            flexDirection: 'column',
            backgroundColor: theme.bg,
            overflowY: 'clip',
          }}
        >
          {!useIsFullscreen() && (
            <div>
              <button onClick={() => rootRef.current?.requestFullscreen()}>⛶</button>
              <UIConfig updateTheme={updateTheme} />
            </div>
          )}
          <div
            style={{
              flex: 1,
              flexDirection: 'column',
              justifyContent: 'space-around',
              position: 'relative',
            }}
          >
            <Tracks />
            {/* <Browser /> */}
            <Bindings />
          </div>
        </div>
      </AppStateContext.Provider>
    </Theme>
  );
};

type UIConfigProps = {
  updateTheme: <K extends keyof ThemeType>(
    k: K
  ) => {
    value: ThemeType[K];
    onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  };
};
const UIConfig = ({ updateTheme }: UIConfigProps) => {
  return (
    <div>
      <input type="color" {...updateTheme('bg')} />
      <input type="color" {...updateTheme('fg')} />
      <input type="range" min="0" max="1" step="0.01" {...updateTheme('distance')} />
      <input type="range" min="0" max="2" step="0.02" {...updateTheme('blur')} />
      <input type="range" min="0" max="1" step="0.01" {...updateTheme('intensity')} />
      <input type="range" min="0.5" max="3" step="0.01" {...updateTheme('space')} />
    </div>
  );
};

const Tracks = () => {
  const state = useAppState();
  if (state === null) return null;
  return (
    <Box
      style={{
        justifyContent: 'space-around',
        alignItems: 'flex-start',
      }}
      neuShadow={false}
      space="lg"
    >
      {state.tracks
        .filter(
          (track) =>
            ['Effect', 'Master'].every((type) => type !== track.type) &&
            ['loop', 'layer', 'metronome'].every((name) => !track.name.startsWith(name))
        )
        .map((track) => (
          <Box
            key={Math.random()}
            style={{
              flex: 1,
              flexDirection: 'column',
              justifyContent: 'flex-start',
            }}
            space="md"
            neuShadow={{ inset: track.isSelected }}
          >
            <Text>{track.name}</Text>
            {track.devices.map((device) => (
              <Box
                neuShadow={{
                  distance: 0.15,
                  intensity: 0.3,
                  blur: 0.3,
                  inset: device.isSelected,
                }}
                space="sm"
              >
                <Text size="md">
                  {device.presetName === 'init' ? device.deviceName : device.presetName}
                </Text>
              </Box>
            ))}
          </Box>
        ))}
    </Box>
  );
};

const Loops = () => {
  const theme = useTheme();
  const loops = useAppState()?.loops;
  if (loops === undefined || loops.length === 0) return null;
  return (
    <div style={{ flex: 1, justifyContent: 'space-around' }}>
      <Box neuShadow={false} style={{ flex: 1, flexDirection: 'column' }}>
        {loops.map((loop) => (
          <Box></Box>
        ))}
      </Box>
    </div>
  );
};

const Bindings = () => {
  const theme = useTheme();
  const state = useAppState();
  if (state === null) return null;
  return (
    <div
      style={{
        flex: 2,
        flexDirection: 'column',
        justifyContent: 'space-around',
      }}
    >
      <Box
        neuShadow={false}
        space="lg"
        style={{
          alignItems: 'stretch',
        }}
      >
        <div
          style={{
            width: '50%',
            display: 'grid',
            grid: 'auto-flow / repeat(4, 1fr)',
            gap: `${theme.space}vw`,
          }}
        >
          {state.pads.map(({ isPressed, bindings: [binding, ...otherBindings] }) => (
            <Pad
              isPressed={isPressed || binding?.value === 1}
              isHalfPressed={binding?.value === 0.5}
              text={binding?.name}
              topRightText={otherBindings.length > 0 ? otherBindings[0].name : undefined}
            />
          ))}
        </div>
        <div
          style={{
            width: '50%',
            display: 'grid',
            grid: 'auto-flow / repeat(4, 1fr)',
            gap: `${theme.space}vw`,
          }}
        >
          {state.knobs.map(({ bindings }) => {
            const binding = bindings.find((binding) => binding?.name.length > 0);
            return (
              <Knob
                name={binding?.name ?? ''}
                value={binding?.displayedValue}
                percentage={binding?.value}
              />
            );
          })}
        </div>
      </Box>
    </div>
  );
};

type PadProps = {
  isPressed: boolean;
  isHalfPressed: boolean;
  text: string;
  topRightText?: string;
};
const Pad = ({ isPressed, isHalfPressed, text, topRightText }: PadProps) => {
  return (
    <Box
      style={{
        aspectRatio: '1/1',
        flexDirection: 'column',
        gap: 0,
      }}
      space="md"
      neuShadow={{
        inset: isPressed,
        ...(!isHalfPressed
          ? {}
          : {
              distance: 0,
              blur: 0,
            }),
      }}
    >
      {topRightText && (
        <Text style={{ textAlign: 'end' }} size="sm" weight="medium">
          {topRightText}
        </Text>
      )}
      <div style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
        <Text
          style={{
            textAlign: 'center',
            overflowWrap: 'anywhere',
          }}
          scale={6}
        >
          {text}
        </Text>
      </div>
    </Box>
  );
};

type KnobProps = {
  name: string;
  value?: string;
  percentage?: number;
};
const Knob = ({ name, value, percentage }: KnobProps) => {
  const theme = useTheme();

  return (
    <div
      style={{
        position: 'relative',
        aspectRatio: '1/1',
      }}
    >
      <Box
        style={{
          borderRadius: '50%',
          width: '100%',
          height: '100%',
        }}
        space="none"
      >
        <svg style={{ position: 'absolute', inset: '2%', filter: '' }} height="100%" width="100%">
          <circle stroke={theme.bg} fill="transparent" strokeWidth="8%" />
        </svg>
        <div
          style={{
            position: 'absolute',
            borderRadius: '50%',
            inset: '2%',
            flexDirection: 'column',
            justifyContent: 'center',
            ...useNeuBoxShadow({
              distance: 0.2,
              intensity: 0.2,
              inset: true,
            }),
          }}
        >
          <div
            style={{
              position: 'absolute',
              borderRadius: '50%',
              inset: '6%',
              ...useNeuBoxShadow({ distance: 0.1, intensity: 0.2 }),
            }}
          ></div>
          {percentage !== undefined && (
            <div
              style={{
                position: 'absolute',
                width: '100%',
                height: '100%',
                transform: `rotate(${(percentage - 0.5) * 270}deg)`,
              }}
            >
              <div
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 'calc(50% - 2px)',
                  width: '3%',
                  height: '6%',
                  backgroundColor: theme.fg,
                }}
              />
            </div>
          )}
          <Text
            style={{
              textAlign: 'center',
              overflowWrap: 'anywhere',
              padding: '0 1vw',
            }}
            scale={5}
          >
            {transformControlName(name)}
          </Text>
          {value && (
            <Text
              style={{
                textAlign: 'center',
                overflowWrap: 'anywhere',
                padding: '0.5vw 1vw 0',
              }}
              size="sm"
            >
              {value}
            </Text>
          )}
        </div>
      </Box>
    </div>
  );
};

const Browser = () => {
  const browser = useAppState()?.browser;
  if (!browser?.isOpen) return null;

  return (
    <Box
      style={{
        maxHeight: '40vh',
        flex: 1,
        overflowX: 'scroll',
      }}
      space="sm"
    >
      {browserColumn.map((column) => (
        <Box style={{ overflowY: 'scroll', flex: 1, flexDirection: 'column' }} space="sm">
          {browser[column].map((result) => (
            <BrowserResultElement result={result} />
          ))}
        </Box>
      ))}
    </Box>
  );
};

const BrowserResultElement = ({ result }: { result: BrowserResult }) => {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (result.isSelected) {
      ref.current?.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }, [result.isSelected, ref]);

  return (
    <Box
      ref={ref}
      style={{
        padding: 5,
      }}
      neuShadow={{
        distance: 0.2,
        intensity: 0.2,
        inset: result.isSelected,
      }}
    >
      <Text size="sm" style={{ fontWeight: 600 }}>
        {result.name}
      </Text>
    </Box>
  );
};

type BoxProps = React.JSX.IntrinsicElements['div'] & {
  space?: 'none' | 'sm' | 'md' | 'lg';
  neuShadow?: Partial<NeuShadowProps>;
};
const Box = forwardRef<HTMLDivElement, BoxProps>(
  ({ style, neuShadow, space = 'md', ...props }: BoxProps, ref) => {
    const theme = useTheme();
    const spaceDim = {
      none: 0,
      sm: '0.5vw',
      md: '1vw',
      lg: '2vw',
    }[space];

    return (
      <div
        ref={ref}
        style={{
          ...useNeuBoxShadow(neuShadow),
          borderRadius: `${theme.roundness}vw`,
          padding: spaceDim,
          gap: spaceDim,
          ...style,
        }}
        {...props}
      />
    );
  }
);

type TextProps = React.JSX.IntrinsicElements['span'] & {
  size?: 'xs' | 'sm' | 'md' | 'lg';
  weight?: 'light' | 'medium' | 'bold';
  scale?: number;
};
const Text = ({ style, size = 'lg', weight = 'bold', scale, ...props }: TextProps) => {
  const { fg } = useTheme();
  const fontScaleFactor =
    scale && typeof props.children === 'string' && props.children.length > scale
      ? Math.pow(scale / props.children.length, 0.3)
      : 1;
  const fontSize =
    fontScaleFactor * (size === 'xs' ? 1.0 : size === 'sm' ? 1.5 : size === 'md' ? 2 : 2.5);
  const fontWeight = weight === 'light' ? 400 : weight === 'medium' ? 700 : 1000;
  return (
    <span
      style={{
        color: fg,
        fontSize: `${fontSize}vw`,
        lineHeight: 1.1,
        fontWeight,
        ...style,
      }}
      {...props}
    />
  );
};

const Icon = ({ i }: { i: MaterialIconName }) => <span className="material-icons">{i}</span>;

type NeuShadowProps =
  | {
      color: string;
      distance: number;
      blur: number;
      intensity: number;
      inset?: boolean;
    }
  | false;

const useNeuBoxShadow = (props?: Partial<NeuShadowProps>) => {
  const theme = useTheme();
  return neuBoxShadow(props === false ? false : { color: theme.bg, ...theme, ...props });
};

const neuBoxShadow = (props: NeuShadowProps): React.CSSProperties => {
  if (!props) return {};
  const { color, blur, intensity, distance, inset = false } = props;
  const insetStr = inset ? 'inset' : '';
  return {
    boxShadow: `
      ${insetStr} ${distance}vw ${distance}vw ${blur}vw ${luminance(color, -intensity)}, 
      ${insetStr} ${-distance}vw ${-distance}vw ${blur}vw ${luminance(color, intensity)}
    `,
  };
};

// const neuSvgShadow = ({
//   color,
//   blur,
//   intensity,
//   distance,
//   inset = false,
// }: NeuShadowProps) => {};

const luminance = (color: string, intensity: number) => {
  const match = color.match(/^#([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])$/)!;

  const colors = [match[1], match[2], match[3]].map((colorStr) => {
    const color = Math.round(Math.min(Math.max(parseInt(colorStr, 16) * (1 + intensity), 0), 255));
    return color.toString(16).padStart(2, '0');
  });

  return `#${colors.join('')}`;
};

const useWindowDimensions = () => {
  const getWindowDimensions = () => ({
    width: window.innerWidth,
    height: window.innerHeight,
  });

  const [windowDimensions, setWindowDimensions] = useState(getWindowDimensions());

  useEffect(() => {
    const handleResize = () => setWindowDimensions(getWindowDimensions());

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  });

  return windowDimensions;
};

const useIsFullscreen = () => {
  const [isFullscreen, setIsFullscreen] = useState(false);
  useEffect(() => {
    const onChange = () => setIsFullscreen(document.fullscreenElement !== null);
    document.addEventListener('fullscreenchange', onChange);
    return () => document.removeEventListener('fullscreenchange', onChange);
  }, []);
  return isFullscreen;
};

const isDev = () => !process.env.NODE_ENV || process.env.NODE_ENV === 'development';
