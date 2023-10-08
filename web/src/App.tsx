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

const ThemeContext = createContext({
  bg: '#f6f5f4' satisfies Property.Color,
  fg: '#c64600' satisfies Property.Color,
  blur: 15,
  distance: 5,
  intensity: 0.4,
  space: 1,
  roundness: 0.5,
});

const useTheme = () => useContext(ThemeContext);

type ThemeType = ContextType<typeof ThemeContext>;

const Theme = ({
  children,
  ...props
}: React.PropsWithChildren<Partial<ThemeType>>) => (
  <ThemeContext.Provider value={{ ...useTheme(), ...props }}>
    {children}
  </ThemeContext.Provider>
);

type State = {
  pads: Control[];
  knobs: Control[];
  mode: string;
  browserResults: BrowserResult[] | null;
  tracks: Track[];
};
type Track = {
  name: string;
  arm: boolean;
  isSelected: boolean;
  type: 'Group' | 'Instrument' | 'Audio' | 'Hybrid' | 'Effect' | 'Master';
  devices: Device[];
};
type Device = {
  name: string;
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
type Binding = {
  name: string;
  value: number;
  displayedValue: string;
};

const AppStateContext = createContext<State | null>(null);
const useAppState = () => useContext(AppStateContext);

const usePluginConnection = () => {
  const [status, setStatus] = useState<
    'CLOSED' | 'OPEN' | 'RECEIVING' | 'ERROR'
  >('CLOSED');
  const [state, setState] = useState<State | null>(null);
  useEffect(() => {
    const socket = new WebSocket('ws://localhost:8080');
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
  const { state } = usePluginConnection();
  const [theme, setTheme] = useState(useTheme());
  const updateTheme: UIConfigProps['updateTheme'] = (k) => ({
    value: theme[k],
    onChange: (e: React.ChangeEvent<HTMLInputElement>) =>
      setTheme((theme) => ({
        ...theme,
        [k]:
          typeof theme[k] === 'number'
            ? parseFloat(e.target.value)
            : e.target.value,
      })),
  });

  return (
    <Theme {...theme}>
      <AppStateContext.Provider value={state}>
        <div
          style={{
            height: '100vh',
            flexDirection: 'column',
            backgroundColor: theme.bg,
            overflowY: 'clip',
          }}
        >
          {/* <UIConfig updateTheme={updateTheme} /> */}
          <div
            style={{
              flex: 1,
              flexDirection: 'column',
              justifyContent: 'space-around',
              padding: 20,
              rowGap: 20,
              position: 'relative',
            }}
          >
            <Tracks />
            <Browser />
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
      <input
        type="range"
        min="0"
        max="20"
        step="0.5"
        {...updateTheme('distance')}
      />
      <input
        type="range"
        min="0"
        max="30"
        step="0.5"
        {...updateTheme('blur')}
      />
      <input
        type="range"
        min="0"
        max="1"
        step="0.01"
        {...updateTheme('intensity')}
      />
      <input
        type="range"
        min="0.5"
        max="3"
        step="0.01"
        {...updateTheme('space')}
      />
    </div>
  );
};

const Tracks = () => {
  const state = useAppState();
  if (state === null) return null;
  return (
    <Box
      style={{
        flex: 1,
        justifyContent: 'space-around',
        columnGap: 20,
        padding: 0,
      }}
      neuShadow={false}
    >
      {state.tracks.map((track) => (
        <Box
          style={{
            flex: 1,
            flexDirection: 'column',
            justifyContent: 'flex-start',
          }}
          space="sm"
          neuShadow={{ inset: track.isSelected }}
        >
          <Text>{track.name}</Text>
          {track.devices.map((device) => (
            <Box
              neuShadow={{
                distance: 2,
                intensity: 0.3,
                blur: 5,
                inset: device.isSelected,
              }}
              space="sm"
            >
              <Text size="md">{device.name}</Text>
            </Box>
          ))}
        </Box>
      ))}
    </Box>
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
      <div
        style={{
          columnGap: '20px',
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
          {state.pads.map(({ bindings: [binding] }) => (
            <Pad text={binding?.name} />
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
            const binding = bindings.find(
              (binding) => binding?.name.length > 0
            );
            return (
              <Knob text={binding?.name ?? ''} percentage={binding?.value} />
            );
          })}
        </div>
      </div>
    </div>
  );
};

type PadProps = {
  text: string;
};
const Pad = ({ text }: PadProps) => {
  return (
    <div
      style={{
        aspectRatio: '1/1',
        borderRadius: 30,
        ...useNeuBoxShadow(),
      }}
    >
      <Text
        style={{
          margin: 'auto',
          textAlign: 'center',
          overflowWrap: 'anywhere',
        }}
      >
        {text}
      </Text>
    </div>
  );
};

type KnobProps = {
  text: string;
  percentage?: number;
};
const Knob = ({ text, percentage }: KnobProps) => {
  const theme = useTheme();

  return (
    <div
      style={{
        position: 'relative',
        aspectRatio: '1/1',
      }}
    >
      <div
        style={{
          borderRadius: '50%',
          width: '100%',
          height: '100%',
          ...useNeuBoxShadow(),
        }}
      >
        <svg
          style={{ position: 'absolute', inset: '2%', filter: '' }}
          height="100%"
          width="100%"
        >
          <circle stroke={theme.bg} fill="transparent" strokeWidth="8%" />
        </svg>
        <div
          style={{
            position: 'absolute',
            borderRadius: '50%',
            inset: '2%',
            ...useNeuBoxShadow({
              distance: 2,
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
              ...useNeuBoxShadow({ distance: 1, intensity: 0.2 }),
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
              margin: 'auto',
              textAlign: 'center',
              overflowWrap: 'anywhere',
              padding: '1vw',
            }}
            scale={6}
          >
            {text}
          </Text>
        </div>
      </div>
    </div>
  );
};

const Browser = () => {
  const state = useAppState();
  if (!state?.browserResults) return null;

  return (
    <Box
      style={{
        maxHeight: '40vh',
        flex: 1,
        overflowY: 'scroll',
        flexDirection: 'column',
      }}
      space="sm"
    >
      {state?.browserResults.map((result) => (
        <BrowserResultElement result={result} />
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
        distance: 2,
        intensity: 0.2,
        inset: result.isSelected,
      }}
    >
      <Text size="md" style={{ fontWeight: 600 }}>
        {result.name}
      </Text>
    </Box>
  );
};

type BoxProps = React.JSX.IntrinsicElements['div'] & {
  space?: 'sm' | 'md' | 'lg';
  neuShadow?: Partial<NeuShadowProps>;
};
const Box = forwardRef<HTMLDivElement, BoxProps>(
  ({ style, neuShadow, space = 'md', ...props }: BoxProps, ref) => {
    const theme = useTheme();
    const spaceDim = space === 'sm' ? '0.5vw' : space === 'md' ? '1vw' : '2vw';
    return (
      <div
        ref={ref}
        style={{
          ...useNeuBoxShadow(neuShadow),
          borderRadius: `${theme.roundness}vw`,
          padding: spaceDim,
          rowGap: spaceDim,
          columnGap: spaceDim,
          ...style,
        }}
        {...props}
      />
    );
  }
);

type TextProps = React.JSX.IntrinsicElements['span'] & {
  size?: 'sm' | 'md' | 'lg';
  weight?: 'light' | 'medium' | 'bold';
  scale?: number;
};
const Text = ({
  style,
  size = 'lg',
  weight = 'bold',
  scale = 0,
  ...props
}: TextProps) => {
  const { fg } = useTheme();
  const fontScaleFactor =
    scale && typeof props.children === 'string'
      ? Math.pow(scale / props.children.length, 0.3)
      : 1;
  const fontSize =
    fontScaleFactor * (size === 'sm' ? 1 : size === 'md' ? 1.5 : 2.5);
  const fontWeight =
    weight === 'light' ? 400 : weight === 'medium' ? 700 : 1000;
  return (
    <span
      style={{ color: fg, fontSize: `${fontSize}vw`, fontWeight, ...style }}
      {...props}
    />
  );
};

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
  return neuBoxShadow(
    props === false ? false : { color: theme.bg, ...theme, ...props }
  );
};

const neuBoxShadow = (props: NeuShadowProps): React.CSSProperties => {
  if (!props) return {};
  const { color, blur, intensity, distance, inset = false } = props;
  const insetStr = inset ? 'inset' : '';
  return {
    boxShadow: `
      ${insetStr} ${distance}px ${distance}px ${blur}px ${luminance(
      color,
      -intensity
    )}, 
      ${insetStr} ${-distance}px ${-distance}px ${blur}px ${luminance(
      color,
      intensity
    )}
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
  const match = color.match(
    /^#([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])$/
  )!;

  const colors = [match[1], match[2], match[3]].map((colorStr) => {
    const color = Math.round(
      Math.min(Math.max(parseInt(colorStr, 16) * (1 + intensity), 0), 255)
    );
    return color.toString(16).padStart(2, '0');
  });

  return `#${colors.join('')}`;
};
